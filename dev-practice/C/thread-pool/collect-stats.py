#!/bin/env python3

import sys, os
import subprocess
import json

class Main(object):
    @staticmethod
    def main():
        exit_code: int = 0
        if len(sys.argv) < 2:
            Main.print_usage()
            return 1
        Main.collect_statistics()
        return exit_code

    @staticmethod
    def print_usage():
        sys.stderr.write("Usage: {} <binary-file>\n".format(sys.argv[0]))

    @staticmethod
    def collect_statistics():
        child = subprocess.run([sys.argv[1]], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        counts = dict()
        for line in child.stdout.decode('utf8').splitlines():
            data = json.loads(line)
            counts[data['thread-id']] = data['contributions']

        for key in sorted(counts.keys()):
            sys.stdout.write("Thread [{}] contributed {} times\n".format(key, counts[key]))

if __name__ == '__main__':
    sys.exit(Main.main())
