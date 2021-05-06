#!/usr/bin/python

# Imports
import sys, os
import argparse
import shutil

# Global variables
exec_name = os.path.basename(__file__)
exec_path = os.path.realpath(__file__)
exec_dir = os.path.dirname(os.path.realpath(__file__))
original_cwd = os.getcwd()

# Main Class
class Main:
    # Class fields
    argv = None
    parser = None

    @staticmethod
    def main():
        # Define method variables
        exit_code = 0

        # Parse arguments
        Main.parse_args()

        # Check for source file
        if Main.argv.source is None:
            Main.parser.print_help()
            return 1

        # Check for destination file
        if Main.argv.destination is None:
            Main.parser.print_help()
            return 1

        # Check if source file exists
        if not os.path.exists(Main.argv.source):
            print("No such file or directory", file=sys.stderr)
            return 1

        # Copy source file to destination
        shutil.copyfile(Main.argv.source, Main.argv.destination)
        

        # End main method
        return exit_code

    @staticmethod
    def parse_args():
        # Define method variables
        Main.parser = argparse.ArgumentParser(description="A simple tool to copy files using Python.")

        # Define arguments to handle
        Main.parser.add_argument('-s', '--source', help="Specify the source file")
        Main.parser.add_argument('-d', '--destination', help="Specify the destination")

        # Parse args
        Main.argv = Main.parser.parse_args()

# Execute
if __name__ == '__main__':
    sys.exit(Main.main())
