#!/usr/bin/python

# Imports
import sys, os
import argparse

# Global variables
exec_name = os.path.basename(__file__)
exec_path = os.path.realpath(__file__)
exec_dir = os.path.dirname(os.path.realpath(__file__))
original_cwd = os.getcwd()

# Main Class
class Main:
    @staticmethod
    def main():
        # Define method variables
        exit_code = 0

        # Parse arguments
        Main.parse_args()

        # End main method
        return exit_code

    @staticmethod
    def parse_args():
        # Define method variables
        parser = argparse.ArgumentParser(description="A simple tool to copy files using Python.")

        # Define arguments to handle
        parser.add_argument('-s', '--source', help="Specify the source file")
        parser.add_argument('-d', '--destination', help="Specify the destination")

# Execute
if __name__ == '__main__':
    sys.exit(Main.main())
