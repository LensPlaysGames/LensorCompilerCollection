#include <codegen.h>
#include <error.h>
#include <locale.h>
#include <parser.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <typechecker.h>
#include <platform.h>

void print_usage(char **argv) {
  print("\nUSAGE: %s [FLAGS] [OPTIONS] <path to file to compile>\n", argv[0]);
  print("Flags:\n"
         "   `-h`, `--help`    :: Show this help and usage information.\n"
         "   `--formats`       :: List acceptable output formats.\n"
         "   `--callings`      :: List acceptable calling conventions.\n"
         "   `--dialects`      :: List acceptable assembly dialects.\n"
         "   `--debug-ir`      :: Dump IR to stdout (in debug format).\n"
         "   `--codegen-only`  :: When --debug-ir is enabled: Dump IR after codegen.\n"
         "   `--print-ast      :: Print the AST and exit.\n"
         "   `--syntax-only    :: Perform no semantic analysis.\n"
         "   `--print-scopes   :: Print the scope tree and exit.\n"
         "   `-O`, `--optimize`:: Optimize the generated code.\n"
         "   `-v`, `--verbose` :: Print out more information.\n");
  print("Options:\n"
         "    `-o`, `--output`   :: Set the output filepath to the one given.\n"
         "    `-f`, `--format`   :: Set the output format to the one given.\n"
         "    `-cc`, `--calling` :: Set the calling convention to the one given.\n"
         "    `-d`, `--dialect`  :: Set the output assembly dialect to the one given.\n"
         "    `--colours`        :: Set whether to use colours in diagnostics.\n"
         "Anything other arguments are treated as input filepaths (source code).\n");
}

int input_filepath_index = -1;
int output_filepath_index = -1;
enum CodegenOutputFormat output_format = CG_FMT_DEFAULT;
enum CodegenCallingConvention output_calling_convention = CG_CALL_CONV_DEFAULT;
enum CodegenAssemblyDialect output_assembly_dialect = CG_ASM_DIALECT_DEFAULT;

int verbosity = 0;
int optimise = 0;
bool debug_ir = false;
bool print_ast = false;
bool syntax_only = false;
bool print_scopes = false;
bool prefer_using_diagnostics_colours = true;
bool colours_blink = false;
bool codegen_only = false;

void print_acceptable_formats() {
  print("Acceptable formats include:\n"
         " -> default\n"
         " -> x86_64_gas\n"
         " -> ir\n");
}

void print_acceptable_calling_conventions() {
  print("Acceptable calling conventions include:\n"
         " -> default\n"
         " -> LINUX\n"
         " -> MSWIN\n");
}

void print_acceptable_asm_dialects() {
  print("Acceptable dialects include:\n"
         " -> default\n"
         " -> att\n"
         " -> intel\n");
}

void print_acceptable_colour_settings() {
  print("Acceptable values for `--colours` include:\n"
         " -> auto\n"
         " -> always\n"
         " -> blink\n"
         " -> never\n");
}

/// @return Zero if everything goes well, otherwise return non-zero value.
int handle_command_line_arguments(int argc, char **argv) {
  /// Default settings.
  prefer_using_diagnostics_colours = platform_isatty(fileno(stdout));

  for (int i = 1; i < argc; ++i) {
    char *argument = argv[i];

    //print("argument %d: \"%s\"\n", i, argument);

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
    } else if (strcmp(argument, "--codegen-only") == 0) {
      codegen_only = true;
    } else if (strcmp(argument, "--debug-ir") == 0) {
      debug_ir = true;
    } else if (strcmp(argument, "--print-ast") == 0) {
      print_ast = true;
    } else if (strcmp(argument, "--print-scopes") == 0) {
      print_scopes = true;
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
        ICE("Expected filepath after output command line argument");
      }
      /// Anything that starts w/ `-` is treated as a command line argument.
      /// If the user has a filepath that starts w/ `-...`, then they should use
      /// `./-...` instead.
      if (*argv[i] == '-') {
        ICE("Expected filepath after output command line argument\n"
               "Instead, got what looks like another command line argument.\n"
               " -> \"%s\"", argv[i]);
      }
      output_filepath_index = i;
    } else if (strcmp(argument, "-f") == 0
               || strcmp(argument, "--format") == 0) {
      i++;
      if (i >= argc) {
        ICE("Expected format after format command line argument");
      }
      if (*argv[i] == '-') {
        ICE("Expected format after format command line argument\n"
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
        print("Expected format after format command line argument\n"
               "Instead, got an unrecognized format: \"%s\".\n", argv[i]);
        print_acceptable_formats();
        return 1;
      }
    } else if (strcmp(argument, "--colours") == 0) {
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
        print("Expected calling convention after calling convention command line argument\n"
               "Instead, got an unrecognized format: \"%s\".\n", argv[i]);
        print_acceptable_calling_conventions();
        return 1;
      }
    } else if (strcmp(argument, "-cc") == 0
               || strcmp(argument, "--calling") == 0) {
      i++;
      if (i >= argc) {
        ICE("Expected calling convention after format command line argument");
      }
      if (*argv[i] == '-') {
        ICE("Expected calling convention after format command line argument\n"
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
        print("Expected calling convention after calling convention command line argument\n"
               "Instead, got an unrecognized format: \"%s\".\n", argv[i]);
        print_acceptable_calling_conventions();
        return 1;
      }
    } else if (strcmp(argument, "-d") == 0
               || strcmp(argument, "--dialect") == 0) {
      i++;
      if (i >= argc) {
        ICE("Expected assembly dialect after format command line argument");
      }
      if (*argv[i] == '-') {
        ICE("Expected assembly dialect after format command line argument\n"
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
        print("Expected assembly dialect after calling convention command line argument\n"
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

    /// Perform semantic analysis.
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
  }

  /// Free the input file.
  free(s.data);

  /// Done!
  if (verbosity) print("\nGenerated code at output filepath \"%s\"\n", output_filepath);
}
