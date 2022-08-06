#ifndef COMPILER_FILE_IO_H
#define COMPILER_FILE_IO_H

#include <stdio.h>

long file_size(FILE *file);
char *file_contents(char *path);

#endif /* COMPILER_FILE_IO_H */
