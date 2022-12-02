#ifndef COMPILER_FILE_IO_H
#define COMPILER_FILE_IO_H

#include <error.h>
#include <stddef.h>
#include <stdio.h>

size_t file_size(FILE *file);
string file_contents(const char *path);

#endif /* COMPILER_FILE_IO_H */
