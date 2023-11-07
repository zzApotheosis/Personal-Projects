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

#define ANIMAL_NAME_SIZE 64
#define ANIMAL_DESCRIPTION "I'm %s. I'm a %d year old %s. %s\n"

struct animal {
        char name[ANIMAL_NAME_SIZE];
        unsigned int age;
        void (* describe)(const struct animal * const);
};

void animal_set_name(struct animal * const self, const char * const new_name) {
        if (self == NULL)
                return;
        if (new_name == NULL)
                return;
        strncpy(self->name, new_name, ANIMAL_NAME_SIZE);
}

void animal_set_age(struct animal * const self, const unsigned int new_age) {
        if (self == NULL)
                return;
        self->age = new_age;
}

void cat_describe(const struct animal * const self) {
        if (self == NULL)
                return;
        fprintf(stdout, ANIMAL_DESCRIPTION, self->name, self->age, "cat", "Meow!");
}

void dog_describe(const struct animal * const self) {
        if (self == NULL)
                return;
        fprintf(stdout, ANIMAL_DESCRIPTION, self->name, self->age, "dog", "BARK!");
}

void mountain_lion_describe(const struct animal * const self) {
        if (self == NULL)
                return;
        fprintf(stdout, ANIMAL_DESCRIPTION, self->name, self->age, "sentient mountain lion", "Let's meat.");
}

struct animal * cat_new() {
        struct animal * instance = (struct animal *) malloc(1 * sizeof(struct animal));
        memset(instance, 0, sizeof(struct animal));
        instance->describe = cat_describe;
        return instance;
}

struct animal * dog_new() {
        struct animal * instance = (struct animal *) malloc(1 * sizeof(struct animal));
        memset(instance, 0, sizeof(struct animal));
        instance->describe = dog_describe;
        return instance;
}

struct animal * mountain_lion_new() {
        struct animal * instance = (struct animal *) malloc(1 * sizeof(struct animal));
        memset(instance, 0, sizeof(struct animal));
        instance->describe = mountain_lion_describe;
        return instance;
}

void animal_free(struct animal * const self) {
        if (self == NULL)
                return;
        free(self);
}

int main(int argc, char * argv[]) {
        // Define method variables
        int exit_code = 0;
        struct animal * cat = cat_new();
        struct animal * dog = dog_new();
        struct animal * mountain_lion = mountain_lion_new();

        animal_set_name(cat, "Angel");
        animal_set_name(dog, "Copper");
        animal_set_name(mountain_lion, "Symba");
        animal_set_age(cat, 7);
        animal_set_age(dog, 3);
        animal_set_age(mountain_lion, 2);

        cat->describe(cat);
        dog->describe(dog);
        mountain_lion->describe(mountain_lion);

        // Now this is just dumb
        fprintf(stdout, "\nHILARIOUSLY INCORRECT USAGE OF C \"POLYMORPHISM\":\n");
        cat->describe(dog);
        dog->describe(mountain_lion);
        mountain_lion->describe(cat);
        
        animal_free(cat);
        animal_free(dog);
        animal_free(mountain_lion);
        
        // Done
        return exit_code;
}
