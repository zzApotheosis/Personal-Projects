#!/bin/env python3

# Imports
import sys, os
from shapes.Circle import Circle
from shapes.Square import Square
from shapes.Triangle import Triangle

# Main Class
class Main(object):
    # Class fields
    exec_name: str = os.path.basename(__file__)
    exec_path: str = os.path.realpath(__file__)
    exec_dir: str = os.path.dirname(os.path.realpath(__file__))
    original_cwd: str = os.getcwd()

    # Main method
    @staticmethod
    def main():
        main = Main()
        main.run()
        return main.result()

    # Constructor
    def __init__(self):
        super(Main, self).__init__()
        self.__result = 0

    # Run method
    def run(self):
        shapes = list()
        circle: Circle = Circle()
        triangle: Triangle = Triangle()
        square: Square = Square()
        shapes.append(circle)
        shapes.append(triangle)
        shapes.append(square)

        for shape in shapes:
            shape.shape()

    # Result method
    def result(self):
        return self.__result

# Execute
if __name__ == '__main__':
    sys.exit(Main.main())
