# Compiler and flags
CC       := gcc
CFLAGS   := -Wall -Wextra -Werror -std=c11
CPPFLAGS := -Iinclude
LDFLAGS  :=
LDLIBS   :=

# Project name
NAME := my_program

# Directories
SRC_DIR := src
OBJ_DIR := build
DEP_DIR := $(OBJ_DIR)/deps

# Source and generated files
SRCS := $(wildcard $(SRC_DIR)/*.c)
OBJS := $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)
DEPS := $(OBJS:.o=.d)

.PHONY: all clean fclean re debug

all: $(NAME)

# Link the executable
$(NAME): $(OBJS)
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

# Compile each source file and generate dependency files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -MP -c $< -o $@

# Include automatically generated header dependencies
-include $(DEPS)

# Debug build
debug: CFLAGS += -g -O0
debug: re

clean:
	rm -rf $(OBJ_DIR)

fclean: clean
	rm -f $(NAME)

re: fclean all
