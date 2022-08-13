#ifndef COMPILER_FILE_IO_H
#define COMPILER_FILE_IO_H

#include <stddef.h>
#include <stdio.h>

size_t file_size(FILE *file);
char *file_contents(char *path);

#endif /* COMPILER_FILE_IO_H */
