#!/usr/bin/python

# Imports
import sys

# Main
class Main(object):
    # Class imports
    import os

    # Class fields
    exec_name: str = os.path.basename(__file__)
    exec_path: str = os.path.realpath(__file__)
    exec_dir: str = os.path.dirname(os.path.realpath(__file__))
    original_cwd: str = os.getcwd()

    # Main
    @staticmethod
    def main():
        # Define method variables
        exit_code = 0
        
        # Do code
        Main()
        
        # Done
        return exit_code

    # Constructor
    def __init__(self):
        super(Main, self).__init__()
        sys.stdout.write("Executable name: " + Main.exec_name + "\n")
        sys.stdout.write("Executable path: " + Main.exec_path + "\n")
        sys.stdout.write("Executable dir: " + Main.exec_dir + "\n")
        sys.stdout.write("Original cwd: " + Main.original_cwd + "\n")

# Execute Main
if __name__ == "__main__":
    sys.exit(Main.main())

