#!/bin/env python3

import sys

class Main(object):
    import os

    exec_path: str = os.path.realpath(__name__)
    exec_dir: str = os.path.dirname(os.path.realpath(__name__))
    original_cwd: str = os.getcwd()

    @staticmethod
    def main():
        exit_code: int = 0
        return exit_code

if __name__ == '__main__':
    sys.exit(Main.main())
