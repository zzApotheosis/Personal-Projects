/*
 * This is a really barebones way to implement object-oriented polymorphism in C.
 *
 * Obviously polymorphism and object-oriented principles in general are more
 * clearly defined and embraced in other languages, but this is more of a
 * demonstration that C is perfectly capable of anything that any other
 * language can do. After all, no amount of fancy language features will
 * change the fact that the machine understands binary instructions and
 * nothing more.
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#define handle_error(msg) do {\
        fprintf(stderr, "Line %d: %s\n", __LINE__, msg);\
        abort();\
} while (0);

#define ANIMAL_NAME_SIZE 64
#define ANIMAL_DESCRIPTION "I'm %s. I'm a %d year old %s. %s\n"

typedef struct animal * animal;
struct animal {
        char name[ANIMAL_NAME_SIZE];
        unsigned int age;

        // Implementation-specific functions
        void (* describe)(const animal);
};

void animal_set_name(animal self, const char * const new_name) {
        if (self == NULL)
                handle_error("self == NULL");
        if (new_name == NULL)
                handle_error("new_name == NULL");
        strncpy(self->name, new_name, ANIMAL_NAME_SIZE);
}

void animal_set_age(animal self, const unsigned int new_age) {
        if (self == NULL)
                handle_error("self == NULL");
        self->age = new_age;
}

void animal_birthday(animal self) {
        if (self == NULL)
                handle_error("self == NULL");
        self->age++;
        fprintf(stdout, "Happy birthday %s! You are now %d years old.\n", self->name, self->age);
}

void animal_describe(const animal self) {
        if (self == NULL)
                handle_error("self == NULL");
        self->describe(self);
}

void cat_describe(const animal self) {
        if (self == NULL)
                handle_error("self == NULL");
        fprintf(stdout, ANIMAL_DESCRIPTION, self->name, self->age, "cat", "Meow!");
}

void dog_describe(const animal self) {
        if (self == NULL)
                handle_error("self == NULL");
        fprintf(stdout, ANIMAL_DESCRIPTION, self->name, self->age, "dog", "BARK!");
}

void mountain_lion_describe(const animal self) {
        if (self == NULL)
                handle_error("self == NULL");
        fprintf(stdout, ANIMAL_DESCRIPTION, self->name, self->age, "sentient mountain lion", "Let's meat.");
}

animal animal_new() {
        animal instance = (animal) malloc(1 * sizeof(struct animal));
        if (instance == NULL)
                handle_error("Failed to allocate memory");
        memset(instance, 0, sizeof(struct animal));
        return instance;
}

animal cat_new() {
        animal instance = animal_new();
        instance->describe = cat_describe;
        return instance;
}

animal dog_new() {
        animal instance = animal_new();
        instance->describe = dog_describe;
        return instance;
}

animal mountain_lion_new() {
        animal instance = animal_new();
        instance->describe = mountain_lion_describe;
        return instance;
}

void animal_free(animal self) {
        if (self == NULL)
                handle_error("self == NULL");
        free(self);
}

int main(int argc, char * argv[]) {
        int exit_code = 0;
        animal cat = cat_new();
        animal dog = dog_new();
        animal mountain_lion = mountain_lion_new();

        animal_set_name(cat, "Angel");
        animal_set_name(dog, "Copper");
        animal_set_name(mountain_lion, "Symba");
        animal_set_age(cat, 7);
        animal_set_age(dog, 2);
        animal_set_age(mountain_lion, 2);

        animal_describe(cat);
        animal_describe(dog);
        animal_describe(mountain_lion);

        animal_birthday(cat);
        animal_birthday(dog);
        animal_birthday(mountain_lion);

        animal_free(cat);
        animal_free(dog);
        animal_free(mountain_lion);
        
        // Done
        return exit_code;
}
