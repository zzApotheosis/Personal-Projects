#!/usr/bin/python

# Imports
import sys, os

# Global variables
exec_name = os.path.basename(__file__)
exec_path = os.path.realpath(__file__)
exec_dir = os.path.dirname(os.path.realpath(__file__))
original_cwd = os.getcwd()

# Main
class Main:
    @staticmethod
    def main():
        # Define method variables
        exit_code = 0
        
        # Do code
        sys.stdout.write("Executable name: " + exec_name + "\n")
        sys.stdout.write("Executable path: " + exec_path + "\n")
        sys.stdout.write("Executable dir: " + exec_dir + "\n")
        sys.stdout.write("Original cwd: " + original_cwd + "\n")
        
        # Done
        return exit_code

# Execute
if __name__ == "__main__":
    sys.exit(Main.main())

