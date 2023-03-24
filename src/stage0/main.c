#include <stdio.h>
#include <stdlib.h>

void lisp(const char*);

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: ./yl [FILENAME]\n");
        return 1;
    }

    char *filename = argv[1];
    FILE *fp = fopen(filename, "r");

    if (fp == NULL) {
        printf("Error: Could not read file %s\n", filename);
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *file_contents = malloc(file_size + 1);
    fread(file_contents, 1, file_size, fp);
    file_contents[file_size] = '\0';

    fclose(fp);

    lisp(file_contents);

    free(file_contents);
    return 0;
}
