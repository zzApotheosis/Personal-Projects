#!/usr/bin/python

# Execute this script with: valgrind ./memleak.py

# Imports
import sys, os

# Global variables
script_name = os.path.basename(__file__)
script_path = os.path.realpath(__file__)
script_dir = os.path.dirname(os.path.realpath(__file__))
original_cwd = os.getcwd()

# Main
class Main:
    @staticmethod
    def main():
        # Define method variables
        exit_code = 0
        
        # Test
        sys.stdout.write("Hi\n")

        # Done
        return exit_code

# Execute
if __name__ == "__main__":
    sys.exit(Main.main())

