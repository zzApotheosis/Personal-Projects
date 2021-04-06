#include <stdlib.h>
#include <stdio.h>

struct person {
    int age;
    float weight;
    char name[256];
};

int main(int argc, char** argv) {
    struct person *p;
    p = (struct person*) malloc(sizeof(struct person));
    p->age = 26;
    p->name = "Steven Jennings";
    p->weight = 180;
    fprintf(stdout, "Name: %s\n", p->name);
    fprintf(stdout, "Age: %d\n", p->age);
    fprintf(stdout, "Weight: %f\n", p->weight);
    free(p);
}

