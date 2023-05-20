#include <codegen.h>
#include <error.h>
#include <locale.h>
#include <parser.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <typechecker.h>
#include <platform.h>
#include <utils.h>

static void print_usage(char **argv) {
  print("\nUSAGE: %s [FLAGS] [OPTIONS] <path to file to compile>\n", 0[argv]);
  print("Flags:\n"
        "   `-h`, `--help`      :: Show this help and usage information.\n"
        "   `-as`, `--archs`    :: List acceptable architectures.\n"
        "   `-ts`, `--targets`  :: List acceptable targets.\n"
        "   `-ccs`, `--callings`:: List acceptable calling conventions.\n"
        "   `--debug-ir`        :: Dump IR to stdout (in debug format).\n"
        "   `--print-ast        :: Print the AST and exit.\n"
        "   `--syntax-only      :: Perform no semantic analysis.\n"
        "   `--print-scopes     :: Print the scope tree and exit.\n"
        "   `--annotate-code    :: Print comments in generated code. TODO: WIP\n"
        "   `-O`, `--optimize`  :: Optimize the generated code.\n"
        "   `-v`, `--verbose`   :: Print out more information.\n");
  print("Options:\n"
        "    `-o`, `--output`   :: Set the output filepath to the one given.\n"
        "    `-a`, `--arch`     :: Set the output architecture to the one given.\n"
        "    `-t`, `--target`   :: Set the output target to the one given.\n"
        "    `-cc`, `--calling` :: Set the calling convention to the one given.\n"
        "    `--colours`        :: Set whether to use colours in diagnostics.\n"
        "Anything other arguments are treated as input filepaths (source code).\n");
}

int input_filepath_index = -1;
int output_filepath_index = -1;
CodegenArchitecture output_arch = ARCH_DEFAULT;
CodegenTarget output_target = TARGET_DEFAULT;
enum CodegenCallingConvention output_calling_convention = CG_CALL_CONV_DEFAULT;

int verbosity = 0;
int optimise = 0;
bool debug_ir = false;
bool print_ast = false;
bool syntax_only = false;
bool print_scopes = false;
bool prefer_using_diagnostics_colours = true;
bool colours_blink = false;
bool annotate_code = false;

static void print_acceptable_architectures() {
  STATIC_ASSERT(ARCH_COUNT == 2, "Exhaustive handling of architectures when printing out all available");
  print("Acceptable architectures include:\n"
         " -> default\n"
         " -> x86_64\n");
}

static void print_acceptable_targets() {
  STATIC_ASSERT(TARGET_COUNT == 5, "Exhaustive handling of targets when printing out all available");
  print("Acceptable targets include:\n"
        " -> default\n"
        " -> asm, assembly\n"
        " -> asm:intel\n"
        "  -> elf_object\n"
        "  -> coff_object\n");
}

static void print_acceptable_calling_conventions() {
  print("Acceptable calling conventions include:\n"
         " -> default\n"
         " -> SYSV, LINUX\n"
         " -> MSWIN\n");
}

static void print_acceptable_colour_settings() {
  print("Acceptable values for `--colours` include:\n"
         " -> auto\n"
         " -> always\n"
         " -> blink\n"
         " -> never\n");
}

/// @return Zero if everything goes well, otherwise return non-zero value.
static int handle_command_line_arguments(int argc, char **argv) {
  /// Default settings.
  prefer_using_diagnostics_colours = platform_isatty(fileno(stdout));

  for (int i = 1; i < argc; ++i) {
    char *argument = i[argv];

    //print("argument %d: \"%s\"\n", i, argument);

    if (strcmp(argument, "-h") == 0
        || strcmp(argument, "--help") == 0) {
      print_usage(argv);
      exit(0);
    } else if (strcmp(argument, "--debug-ir") == 0) {
      debug_ir = true;
    } else if (strcmp(argument, "--print-ast") == 0) {
      print_ast = true;
    } else if (strcmp(argument, "--print-scopes") == 0) {
      print_scopes = true;
    } else if (strcmp(argument, "--syntax-only") == 0) {
      syntax_only = true;
    } else if (strcmp(argument, "--annotate-code") == 0) {
      annotate_code = true;
    } else if (strcmp(argument, "-O") == 0
               || strcmp(argument, "--optimise") == 0) {
      optimise = 1;
    }  else if (strcmp(argument, "-v") == 0
               || strcmp(argument, "--verbose") == 0) {
      verbosity = 1;
    } else if (strcmp(argument, "-as") == 0 || strcmp(argument, "--archs") == 0) {
      print_acceptable_architectures();
      exit(0);
    } else if (strcmp(argument, "-a") == 0 || strcmp(argument, "--arch") == 0) {
      i++;
      if (i >= argc)
        ICE("Expected architecture after command line argument %s", argument);

      /// Anything that starts w/ `-` is treated as a command line argument.
      /// If the user has a filepath that starts w/ `-...`, then they should use
      /// `./-...` instead.
      if (0[i[argv]] == '-') {
        ICE("Expected architecture after command line argument %s\n"
               "Instead, got what looks like another command line argument.\n"
               " -> \"%s\"", argument, argv[i]);
      }
      STATIC_ASSERT(ARCH_COUNT == 2, "Exhaustive handling of architecture count in command line argument parsing");
      if (strcmp(argv[i], "default") == 0) {
        output_arch = ARCH_DEFAULT;
      } else if (strcmp(argv[i], "x86_64_gas") == 0) {
        output_arch = ARCH_X86_64;
      } else {
        print("Expected architecture after command line argument %s\n"
               "Instead, got unrecognized: \"%s\".\n", argument, argv[i]);
        print_acceptable_architectures();
        return 1;
      }
    } else if (strcmp(argument, "--targets") == 0) {
      print_acceptable_targets();
      exit(0);
    } else if (strcmp(argument, "-t") == 0 || strcmp(argument, "--target") == 0) {
      i++;
      if (i >= argc)
        ICE("Expected target after command line argument %s", argument);

      /// Anything that starts w/ `-` is treated as a command line argument.
      /// If the user has a filepath that starts w/ `-...`, then they should use
      /// `./-...` instead.
      if (0[i[argv]] == '-') {
        ICE("Expected target after command line argument %s\n"
               "Instead, got what looks like another command line argument.\n"
               " -> \"%s\"", argument, argv[i]);
      }
      STATIC_ASSERT(TARGET_COUNT == 5, "Exhaustive handling of target count in command line argument parsing");
      if (strcmp(argv[i], "default") == 0) {
        output_target = TARGET_DEFAULT;
      } else if (strcmp(argv[i], "asm") == 0 || strcmp(argv[i], "assembly") == 0) {
        output_target = TARGET_GNU_ASM_ATT;
      } else if (strcmp(argv[i], "asm:intel") == 0) {
        output_target = TARGET_GNU_ASM_INTEL;
      } else if (strcmp(argv[i], "elf_object") == 0) {
        output_target = TARGET_ELF_OBJECT;
      } else if (strcmp(argv[i], "coff_object") == 0) {
        output_target = TARGET_COFF_OBJECT;
      } else {
        print("Expected architecture after command line argument %s\n"
               "Instead, got unrecognized: \"%s\".\n", argument, argv[i]);
        print_acceptable_architectures();
        return 1;
      }
    } else if (strcmp(argument, "-o") == 0
               || strcmp(argument, "--output") == 0) {
      i++;
      if (i >= argc) {
        ICE("Expected filepath after output command line argument");
      }
      /// Anything that starts w/ `-` is treated as a command line argument.
      /// If the user has a filepath that starts w/ `-...`, then they should use
      /// `./-...` instead.
      if (0[i[argv]] == '-') {
        ICE("Expected filepath after output command line argument\n"
               "Instead, got what looks like another command line argument.\n"
               " -> \"%s\"", argv[i]);
      }
      output_filepath_index = i;
    } else if (strcmp(argument, "--colours") == 0 || strcmp(argument, "--colors") == 0) {
      i++;
      if (i >= argc) {
        fprint(stderr, "Error: Expected option value after `--colours`\n");
        print_acceptable_colour_settings();
        exit(1);
      }
      if (strcmp(argv[i], "auto") == 0) {
        prefer_using_diagnostics_colours = platform_isatty(fileno(stdout));
      } else if (strcmp(argv[i], "never") == 0) {
        prefer_using_diagnostics_colours = false;
      } else if (strcmp(argv[i], "blink") == 0) {
        prefer_using_diagnostics_colours = true;
        colours_blink = true;
      } else if (strcmp(argv[i], "always") == 0) {
        prefer_using_diagnostics_colours = true;
      } else {
        print("Expected colour option after colour option command line argument\n"
               "Instead, got unrecognized: \"%s\".\n", argv[i]);
        print_acceptable_colour_settings();
        return 1;
      }
    } else if (strcmp(argument, "-ccs") == 0 || strcmp(argument, "--callings") == 0) {
      print_acceptable_calling_conventions();
      exit(0);
    } else if (strcmp(argument, "-cc") == 0 || strcmp(argument, "--calling") == 0) {
      i++;
      if (i >= argc) {
        ICE("Expected calling convention after command line argument %s", argument);
      }
      if (*argv[i] == '-') {
        ICE("Expected calling convention after command line argument %s\n"
            "Instead, got what looks like another command line argument.\n"
            " -> \"%s\"", argument, argv[i]);
      }
      STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "Exhaustive handling of calling conventions in command line argument parsing");
      if (strcmp(argv[i], "default") == 0) {
        output_calling_convention = CG_CALL_CONV_DEFAULT;
      } else if (strcmp(argv[i], "MSWIN") == 0) {
        output_calling_convention = CG_CALL_CONV_MSWIN;
      } else if (strcmp(argv[i], "SYSV") == 0 || strcmp(argv[i], "LINUX") == 0) {
        output_calling_convention = CG_CALL_CONV_SYSV;
      } else {
        print("Expected calling convention after command line argument %s\n"
              "Instead, got unrecognized: \"%s\".\n", argument, argv[i]);
        print_acceptable_calling_conventions();
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
      if (input_filepath_index == -1) input_filepath_index = i;
      else ICE("Error: Unrecognized command line argument: \"%s\"", argument);
    }
  }
  return 0;
}

int main(int argc, char **argv) {
  platform_init();

  if (argc < 2) {
    print_usage(argv);
    return 0;
  }

  int status = handle_command_line_arguments(argc, argv);
  if (status) { return status; }
  if (input_filepath_index == -1) {
    print("Input file path was not provided.");
    print_usage(argv);
    return 1;
  }

  thread_use_colours = prefer_using_diagnostics_colours;

  const char *infile = argv[input_filepath_index];
  const char *output_filepath = output_filepath_index == -1 ? "code.S" : argv[output_filepath_index];
  size_t len = strlen(infile);
  bool ok = false;
  string s = platform_read_file(infile, &ok);
  if (!ok) ICE("%S", s);

  /// The input is an IR file.
  if (len >= 3 && memcmp(infile + len - 3, ".ir", 3) == 0) {
    ASSERT(s.data);

    TODO("Development of IR parser and codegen is severely behind right now.");

    if (!codegen(
      LANG_IR,
      output_arch,
      output_target,
      output_calling_convention,
      infile,
      output_filepath,
      NULL,
      s
     )) {
      exit(1);
     }

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
      if (print_scopes) ast_print_scope_tree(stdout, ast);
      goto done;
    }

    /// Perform semantic analysis program expressions.
    ok = typecheck_expression(ast, ast->root);
    if (!ok) exit(2);

    /// Print if requested.
    if (print_ast || print_scopes) {
      if (print_ast) ast_print(stdout, ast);
      if (print_scopes) ast_print_scope_tree(stdout, ast);
    }

    /// Generate code.
    if (!codegen(
      LANG_FUN,
      output_arch,
      output_target,
      output_calling_convention,
      infile,
      output_filepath,
      ast,
      (string){0}
    )) exit(3);

  done:
    ast_free(ast);
  }

  /// Free the input file.
  free(s.data);

  /// Done!
  if (verbosity) print("\nGenerated code at output filepath \"%s\"\n", output_filepath);
}
