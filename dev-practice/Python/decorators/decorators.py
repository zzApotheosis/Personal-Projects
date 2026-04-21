#!/usr/bin/python

# Imports
import sys

# Decorator
def my_decorator(func):
    def wrapper(self, *args, **kwargs):
        print("Something before the function")
        func(self, *args, **kwargs)
        print("Something after the function")
    return wrapper

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
        app = Main()
        app.cool_method(msg = "Hello")
        
        # Done
        return exit_code

    # Constructor
    def __init__(self):
        super(Main, self).__init__()
        sys.stdout.write("Executable name: " + Main.exec_name + "\n")
        sys.stdout.write("Executable path: " + Main.exec_path + "\n")
        sys.stdout.write("Executable dir: " + Main.exec_dir + "\n")
        sys.stdout.write("Original cwd: " + Main.original_cwd + "\n")

    # Decorated method
    @my_decorator
    def cool_method(self, msg=None):
        print("[{}] I'm pretty cool tbh.".format(msg))

# Execute Main
if __name__ == "__main__":
    sys.exit(Main.main())

