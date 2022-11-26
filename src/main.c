#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <codegen.h>
#include <error.h>
#include <environment.h>
#include <file_io.h>
#include <parser.h>
#include <typechecker.h>

void print_usage(char **argv) {
  printf("\nUSAGE: %s [FLAGS] [OPTIONS] <path to file to compile>\n", argv[0]);
  printf("Flags:\n"
         "   `-h`, `--help`    :: Show this help and usage information.\n"
         "   `--formats`       :: List acceptable output formats.\n"
         "   `--callings`      :: List acceptable calling conventions.\n"
         "   `--dialects`      :: List acceptable assembly dialects.\n"
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
int verbosity = 0;
int optimise = 0;

void print_acceptable_formats() {
  printf("Acceptable formats include:\n"
         " -> default\n"
         " -> x86_64_gas\n");
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
        panic("ERROR: Expected filepath after output command line argument");
      }
      // FIXME: This very well may be a valid filepath. We may want to
      //        check that it isn't a valid filepath with fopen or something.
      if (*argv[i] == '-') {
        panic("ERROR: Expected filepath after output command line argument\n"
               "Instead, got what looks like another command line argument.\n"
               " -> \"%s\"", argv[i]);
      }
      output_filepath_index = i;
    } else if (strcmp(argument, "-f") == 0
               || strcmp(argument, "--format") == 0) {
      i++;
      if (i >= argc) {
        panic("ERROR: Expected format after format command line argument");
      }
      if (*argv[i] == '-') {
        panic("ERROR: Expected format after format command line argument\n"
               "Instead, got what looks like another command line argument.\n"
               " -> \"%s\"", argv[i]);
      }
      if (strcmp(argv[i], "default") == 0) {
        output_format = CG_FMT_DEFAULT;
      } else if (strcmp(argv[i], "x86_64_gas") == 0) {
        output_format = CG_FMT_x86_64_GAS;
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
        panic("ERROR: Expected calling convention after format command line argument");
      }
      if (*argv[i] == '-') {
        panic("ERROR: Expected calling convention after format command line argument\n"
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
        panic("ERROR: Expected assembly dialect after format command line argument");
      }
      if (*argv[i] == '-') {
        panic("ERROR: Expected assembly dialect after format command line argument\n"
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

  Node *program = node_allocate();
  ParsingContext *context = parse_context_default_create();
  Error err = parse_program(argv[input_filepath_index], context, program);

  if (verbosity) {
    printf("----- Abstract Syntax Tree\n");
    print_node(program, 0);
    printf("----- Parsing Context\n");
    parse_context_print(context,0);
    printf("-----\n");
  }

  if (err.type) {
    print_error(err);
    return 1;
  }

  err = typecheck_program(context, program);
  if (err.type) {
    print_error(err);
    return 2;
  }

  char *output_filepath = output_filepath_index == -1 ? "code.S" : argv[output_filepath_index];
  err = codegen(output_format, output_calling_convention, output_assembly_dialect, output_filepath, context, program);
  if (err.type) {
    print_error(err);
    return 3;
  }

  printf("\nGenerated code at output filepath \"%s\"\n", output_filepath);

  node_free(program);

  return 0;
}
