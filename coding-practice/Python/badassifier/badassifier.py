#!/bin/env python3

# Imports
import sys, os
import argparse
import random

# Main Class
class Main(object):
    # Class fields
    exec_name: str = os.path.basename(__file__)
    exec_path: str = os.path.realpath(__file__)
    exec_dir: str = os.path.dirname(os.path.realpath(__file__))
    original_cwd: str = os.getcwd()
    min_chance: float = 0.0
    max_chance: float = 1.0
    
    # Main method
    @staticmethod
    def main():
        main = Main()
        main.parse_args()
        main.badassifier()
        return main.result()

    # Constructor
    def __init__(self):
        super(Main, self).__init__()
        self.__badass_word_chance = 0.10
        self.__result = 0

    # Arguments parsing method
    def parse_args(self):
        parser = argparse.ArgumentParser(
            prog = Main.exec_name,
            description = 'A tool to make text extremely badass.',
            epilog = 'Thanks, Earl',
        )
        parser.add_argument('-f', '--file',
                            help = 'A file to read and badassify. Can be specified multiple times. Defaults to stdin.',
                            action = 'append',
                            required = False)
        parser.add_argument('--chance',
                            help = 'A float value representing the chance that a badass word is inserted into the text. Min value = ' + str(Main.min_chance) + ', max value = ' + str(Main.max_chance) + '',
                            action = 'store',
                            required = False)
        parser.add_argument('-o', '--output',
                            help = 'An output file for the badassified text. Defaults to stdout.',
                            action = 'store',
                            required = False)
        args = parser.parse_args()

        self.files = args.file

        if args.chance is not None:
            chance: float = float(args.chance)
            if chance > Main.max_chance or chance < Main.min_chance:
                sys.stderr.write(str(chance) + " is outside the range (" + str(Main.min_chance) + "," + str(Main.max_chance) + "), setting chance to ")
                chance = Main.clamp(chance, min = Main.min_chance, max = Main.max_chance)
                sys.stderr.write(str(chance) + "\n")
            self.__badass_word_chance = chance

        self.__output_file = args.output

    # Badassifier method
    def badassifier(self):
        output = None
        if self.__output_file is not None:
            output = open(self.__output_file, 'w')
        else:
            output = sys.stdout
        
        # Default to stdin if no files were provided
        if self.files is None:
            for line in sys.stdin:
                self.badassify_line(fh = output, line = line)
        else:
            # Open files for reading
            for file in self.files:
                with open(file, 'r') as f:
                    # Iterate over all lines in the file
                    for line in f:
                        self.badassify_line(fh = output, line = line)

        # Close output file if exists
        if self.__output_file is not None:
            output.close()

    # Badassify a given line
    def badassify_line(self, fh = None, line = None):
        tokenized_line = line.split()

        for i in range(0, len(tokenized_line)):
            if i != 0:
                fh.write(" ")
            fh.write(tokenized_line[i])

            if i != len(tokenized_line) - 1 and not Main.sentence_end(tokenized_line[i]) and random.random() < self.__badass_word_chance:
                fh.write(" " + BadassWords.get_word())

        fh.write("\n")
        

    # Result method
    def result(self):
        return self.__result

    # Utility clamp method
    @staticmethod
    def clamp(val, min = None, max = None):
        if min is None:
            sys.stderr.write("Improper usage of " + __func__)
            return val
        if max is None:
            sys.stderr.write("Improper usage of " + __func__)
            return val

        if val < min:
            return min
        if val > max:
            return max
        return val

    # Utility sentence end detection method
    @staticmethod
    def sentence_end(word = None):
        if word.endswith('.'):
            return True
        if word.endswith('!'):
            return True
        if word.endswith('?'):
            return True
        return False

# BadassWords Class
class BadassWords(object):
    #word_list = ('motherfucker', 'goddamn', 'badass', 'fuck', 'bastard', 'fuckin sick', 'motherfuckin', 'piece of shit')
    #word_list = ('motherfucker', 'goddamn', 'badass', 'fuck', 'bastard', 'fuckin sick', 'motherfuckin', 'shit')
    word_list = ('goddamn', 'badass')

    # Get a pretty badass word
    @staticmethod
    def get_word():
        return BadassWords.word_list[random.randint(0, len(BadassWords.word_list) - 1)]

# Execute
if __name__ == '__main__':
    sys.exit(Main.main())
