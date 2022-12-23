#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <codegen.h>
#include <error.h>
#include <file_io.h>
#include <parser.h>
#include <typechecker.h>

#ifndef _WIN32
#include <signal.h>

/// SIGSEGV/SIGILL/SIGABRT handler.
void ice_signal_handler(int signal, siginfo_t *info, void* unused) {
  (void) unused;
  switch (signal) {
    case SIGSEGV: ICE_SIGNAL("Segmentation fault at 0x%lx", (uintptr_t)info->si_addr);
    case SIGILL: ICE_SIGNAL("Illegal instruction");
    case SIGABRT: ICE_SIGNAL("Aborted");
    default: ICE_SIGNAL("UNREACHABLE");
  }
}

#endif

void print_usage(char **argv) {
  printf("\nUSAGE: %s [FLAGS] [OPTIONS] <path to file to compile>\n", argv[0]);
  printf("Flags:\n"
         "   `-h`, `--help`    :: Show this help and usage information.\n"
         "   `--formats`       :: List acceptable output formats.\n"
         "   `--callings`      :: List acceptable calling conventions.\n"
         "   `--dialects`      :: List acceptable assembly dialects.\n"
         "   `--debug-ir`      :: Dump IR to stdout (in debug format).\n"
         "   `--print-ast      :: Print the AST and exit.\n"
         "   `--syntax-only    :: Perform no semantic analysis.\n"
         "   `-O`, `--optimize`:: Optimize the generated code.\n"
         "   `-v`, `--verbose` :: Print out more information.\n");
  printf("Options:\n"
         "    `-o`, `--output`   :: Set the output filepath to the one given.\n"
         "    `-f`, `--format`   :: Set the output format to the one given.\n"
         "    `-cc`, `--calling` :: Set the calling convention to the one given.\n"
         "    `-d`, `--dialect`   :: Set the output assembly dialect to the one given.\n"
         "Anything other arguments are treated as input filepaths (source code).\n");
}

int input_filepath_index = -1;
int output_filepath_index = -1;
enum CodegenOutputFormat output_format = CG_FMT_DEFAULT;
enum CodegenCallingConvention output_calling_convention = CG_CALL_CONV_DEFAULT;
enum CodegenAssemblyDialect output_assembly_dialect = CG_ASM_DIALECT_DEFAULT;

/// TODO: should be bools.
int verbosity = 0;
int optimise = 0;
bool debug_ir = false;
bool print_ast = false;
bool syntax_only = false;

void print_acceptable_formats() {
  printf("Acceptable formats include:\n"
         " -> default\n"
         " -> x86_64_gas\n"
         " -> ir\n");
}

void print_acceptable_calling_conventions() {
  printf("Acceptable calling conventions include:\n"
         " -> default\n"
         " -> LINUX\n"
         " -> MSWIN\n");
}

void print_acceptable_asm_dialects() {
  printf("Acceptable dialects include:\n"
         " -> default\n"
         " -> att\n"
         " -> intel\n");
}

/// @return Zero if everything goes well, otherwise return non-zero value.
int handle_command_line_arguments(int argc, char **argv) {
  for (int i = 1; i < argc; ++i) {
    char *argument = argv[i];

    //printf("argument %d: \"%s\"\n", i, argument);

    if (strcmp(argument, "-h") == 0
        || strcmp(argument, "--help") == 0) {
      print_usage(argv);
      exit(0);
    } else if (strcmp(argument, "--formats") == 0) {
      print_acceptable_formats();
      exit(0);
    } else if (strcmp(argument, "--callings") == 0) {
      print_acceptable_calling_conventions();
      exit(0);
    } else if (strcmp(argument, "--dialects") == 0) {
      print_acceptable_asm_dialects();
      exit(0);
    } else if (strcmp(argument, "--debug-ir") == 0) {
      debug_ir = true;
    } else if (strcmp(argument, "--print-ast") == 0) {
      print_ast = true;
    } else if (strcmp(argument, "--syntax-only") == 0) {
      syntax_only = true;
    } else if (strcmp(argument, "-O") == 0
               || strcmp(argument, "--optimise") == 0) {
      optimise = 1;
    }  else if (strcmp(argument, "-v") == 0
               || strcmp(argument, "--verbose") == 0) {
      verbosity = 1;
    } else if (strcmp(argument, "-o") == 0
               || strcmp(argument, "--output") == 0) {
      i++;
      if (i >= argc) {
        PANIC("ERROR: Expected filepath after output command line argument");
      }
      /// Anything that starts w/ `-` is treated as a command line argument.
      /// If the user has a filepath that starts w/ `-...`, then they should use
      /// `./-...` instead.
      if (*argv[i] == '-') {
        PANIC("ERROR: Expected filepath after output command line argument\n"
               "Instead, got what looks like another command line argument.\n"
               " -> \"%s\"", argv[i]);
      }
      output_filepath_index = i;
    } else if (strcmp(argument, "-f") == 0
               || strcmp(argument, "--format") == 0) {
      i++;
      if (i >= argc) {
        PANIC("ERROR: Expected format after format command line argument");
      }
      if (*argv[i] == '-') {
        PANIC("ERROR: Expected format after format command line argument\n"
               "Instead, got what looks like another command line argument.\n"
               " -> \"%s\"", argv[i]);
      }
      if (strcmp(argv[i], "default") == 0) {
        output_format = CG_FMT_DEFAULT;
      } else if (strcmp(argv[i], "x86_64_gas") == 0) {
        output_format = CG_FMT_x86_64_GAS;
      } else if (strcmp(argv[i], "ir") == 0) {
        output_format = CG_FMT_IR;
      } else {
        printf("ERROR: Expected format after format command line argument\n"
               "Instead, got an unrecognized format: \"%s\".\n", argv[i]);
        print_acceptable_formats();
        return 1;
      }
    } else if (strcmp(argument, "-cc") == 0
               || strcmp(argument, "--calling") == 0) {
      i++;
      if (i >= argc) {
        PANIC("ERROR: Expected calling convention after format command line argument");
      }
      if (*argv[i] == '-') {
        PANIC("ERROR: Expected calling convention after format command line argument\n"
               "Instead, got what looks like another command line argument.\n"
               " -> \"%s\"", argv[i]);
      }
      if (strcmp(argv[i], "default") == 0) {
        output_calling_convention = CG_CALL_CONV_DEFAULT;
      } else if (strcmp(argv[i], "MSWIN") == 0) {
        output_calling_convention = CG_CALL_CONV_MSWIN;
      } else if (strcmp(argv[i], "LINUX") == 0) {
        output_calling_convention = CG_CALL_CONV_LINUX;
      } else {
        printf("ERROR: Expected calling convention after calling convention command line argument\n"
               "Instead, got an unrecognized format: \"%s\".\n", argv[i]);
        print_acceptable_calling_conventions();
        return 1;
      }
    } else if (strcmp(argument, "-d") == 0
               || strcmp(argument, "--dialect") == 0) {
      i++;
      if (i >= argc) {
        PANIC("ERROR: Expected assembly dialect after format command line argument");
      }
      if (*argv[i] == '-') {
        PANIC("ERROR: Expected assembly dialect after format command line argument\n"
              "Instead, got what looks like another command line argument.\n"
              " -> \"%s\"", argv[i]);
      }
      if (strcmp(argv[i], "default") == 0) {
        output_assembly_dialect = CG_ASM_DIALECT_DEFAULT;
      } else if (strcmp(argv[i], "att") == 0) {
        output_assembly_dialect = CG_ASM_DIALECT_ATT;
      } else if (strcmp(argv[i], "intel") == 0) {
        output_assembly_dialect = CG_ASM_DIALECT_INTEL;
      } else {
        printf("ERROR: Expected assembly dialect after calling convention command line argument\n"
               "Instead, got an unrecognized format: \"%s\".\n", argv[i]);
        print_acceptable_asm_dialects();
        return 1;
      }
    } else if (strcmp(argument, "--aluminium") == 0) {
#     if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
      // Windows
      system("start https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#     elif __APPLE__
      // Apple (iOS, OS X, watchOS...)
      system("open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#     elif __linux__ || __unix__
      // Linux or unix-based
      system("xdg-open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#     endif
    } else {
      if (input_filepath_index != -1) {
        printf("ERROR: Only a single input filepath is used, but multiple were given.\n"
               "Using the latest one.\n");
      }
      input_filepath_index = i;
    }
  }
  return 0;
}

int main(int argc, char **argv) {
#ifndef _WIN32
  {
    /// Install signal handlers.
    struct sigaction sa = {0};
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO;
    sa.sa_sigaction = ice_signal_handler;
    if (sigaction(SIGSEGV, &sa, NULL)) ICE("Failed to install SIGSEGV handler");
    if (sigaction(SIGABRT, &sa, NULL)) ICE("Failed to install SIGABRT handler");
    if (sigaction(SIGILL, &sa, NULL)) ICE("Failed to install SIGILL handler");
  }
#endif

  if (argc < 2) {
    print_usage(argv);
    return 0;
  }

  int status = handle_command_line_arguments(argc, argv);
  if (status) { return status; }
  if (input_filepath_index == -1) {
    printf("Input file path was not provided.");
    print_usage(argv);
    return 1;
  }

  const char *infile = argv[input_filepath_index];
  const char *output_filepath = output_filepath_index == -1 ? "code.S" : argv[output_filepath_index];
  size_t len = strlen(infile);
  string s = file_contents(infile);
  if (!s.data) exit(1);

  /// The input is an IR file.
  if (len >= 3 && memcmp(infile + len - 3, ".ir", 3) == 0) {
    ASSERT(s.data);
/*

    if (!codegen(
      LANG_IR,
      output_format,
      output_calling_convention,
      output_assembly_dialect,
      infile,
      output_filepath,
      NULL,
      s
     )) {
      exit(1);
     }
*/

    free(s.data);
  }

  /// The input is a FUN file.
  else {
    /// Parse the file.
    AST *ast = parse(as_span(s), infile);
    if (!ast) exit(1);

    /// Print if requested.
    if (syntax_only) {
      if (print_ast) ast_print(stdout, ast);
      goto done;
    }

    /// Perform semantic analysis.
    bool ok = typecheck_expression(ast, ast->root);
    if (!ok) exit(2);

    /// Print if requested.
    if (print_ast) {
      ast_print(stdout, ast);
      goto done;
    }

    /// Generate code.
    if (!codegen(
      LANG_FUN,
      output_format,
      output_calling_convention,
      output_assembly_dialect,
      infile,
      output_filepath,
      ast,
      (string){0}
    )) exit(3);

  done:
    ast_free(ast);
    return 0;
  }

  if (verbosity) printf("\nGenerated code at output filepath \"%s\"\n", output_filepath);

}
