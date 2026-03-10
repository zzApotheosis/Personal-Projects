#!/bin/env python3

import os, sys
import argparse
import hashlib

class Definitions(object):
    prog_version_major: int = 0
    prog_version_minor: int = 0
    prog_version_patch: int = 1
    default_salt_length: int = 16
    default_iterations: int = 1_000_000
    default_key_length: int = 32
    default_hash_algo: str = 'SHA512'

    @staticmethod
    def prog_version():
        return '{}.{}.{}'.format(Definitions.prog_version_major,
                                 Definitions.prog_version_minor,
                                 Definitions.prog_version_patch)

class AppArgs(argparse.ArgumentParser):
    def __init__(self):
        super(AppArgs, self).__init__(description='A program to derive a key using PBKDF2')
        self.help = None
        self.namespace: argparse.Namespace = None
        self.positionals = list()

    def load_env(self):
        if 'PBKDF2_PASSWORD' in os.environ:
            self.password = os.environ['PBKDF2_PASSWORD']
        if 'PBKDF2_SALT' in os.environ:
            self.password = os.environ['PBKDF2_SALT']
        if 'PBKDF2_ITERATIONS' in os.environ:
            self.iterations = os.environ['PBKDF2_ITERATIONS']
        if 'PBKDF2_KEY_LENGTH' in os.environ:
            self.key_length = os.environ['PBKDF2_KEY_LENGTH']
        if 'PBKDF2_HASH_ALGO' in os.environ:
            self.hash_algorithm = os.environ['PBKDF2_HASH_AGLO']
        
    def parse(self):
        def bail(exit_code: int = 0):
            ap.print_help()
            sys.exit(exit_code)
        #self.add_argument('-h', '--help', help='Show this help prompt and exit', action='store_true')
        self.add_argument('-V', '--version', help='Show the program version and exit', action='store_true')
        self.add_argument('-v', '--verbose', help='Increment the program verbosity level', action='count')
        self.add_argument('-p', '--password', help='The password to use for key derivation', action='store')
        self.add_argument('-s', '--salt', help='The salt file to use for key derivation', action='store')
        self.add_argument('--iterations', help='The number of iterations', action='store')
        self.add_argument('--key-length', help='The length of the derived key', action='store')
        self.add_argument('--hash-algo', help='The hash algorithm to use', action='store')
        self.add_argument('--output-salt', help='The output file for the salt data', action='store')
        self.add_argument('--output-key', help='The output file for the key data', action='store', required=True)
        self.namespace, self.positionals = self.parse_known_args()

        if self.namespace.version:
            sys.stderr.write(Definitions.prog_version() + "\n")
            sys.exit(0)

        # Final check for required arguments
        if self.namespace.password is None:
            bail(1)

class Main(object):
    app_args: AppArgs
    
    @staticmethod
    def main():
        Main.init()
        exit_code = Main.pbkdf2_example()
        return exit_code

    @staticmethod
    def init():
        Main.app_args = AppArgs()
        Main.app_args.load_env()
        Main.app_args.parse()

    @staticmethod
    def pbkdf2_example():
        status: int = 0
        salt_file: str = Main.app_args.namespace.salt
        salt: bytearray = None
        password: bytearray = Main.app_args.namespace.password.encode('utf-8')
        iterations: int = Main.app_args.namespace.iterations
        key_length: int = Main.app_args.namespace.key_length
        hash_algo: str = Main.app_args.namespace.hash_algo
        
        if salt_file is not None:
            with open(salt_file, 'rb') as fh:
                salt = fh.read(Definitions.default_salt_length)
        else:
            salt = PBKDF2.gen_salt(l=Definitions.default_salt_length)
        if iterations is None:
            iterations = Definitions.default_iterations
        if key_length is None:
            key_length = Definitions.default_key_length
        if hash_algo is None:
            hash_algo = Definitions.default_hash_algo

        derived_key = PBKDF2.derive(password=password,
                                    salt=salt,
                                    iterations=iterations,
                                    key_length=key_length,
                                    hash_algorithm=hash_algo)
        
        #print(f"Salt (hex): {salt.hex()}")
        #print(f"Derived Key (hex): {derived_key.hex()}")

        if salt_file is None and Main.app_args.namespace.output_salt is None:
            sys.stderr.write("Warning: No output salt specified\n")
        if Main.app_args.namespace.output_salt is not None:
            with open(Main.app_args.namespace.output_salt, 'w+') as fh:
                fh.buffer.write(salt)
        with open(Main.app_args.namespace.output_key, 'w+') as fh:
            fh.buffer.write(derived_key)

        return status

class AppException(Exception):
    pass

class PBKDF2(object):
    @staticmethod
    def gen_salt(l=None):
        if l is None or l < 1:
            raise AppException("Incorrect usage")
        return os.urandom(l)
        
    @staticmethod
    def derive(password=None, salt=None, iterations=None, key_length=None, hash_algorithm=None):
        if password is None or salt is None or iterations is None or key_length is None or hash_algorithm is None:
            raise AppException("Incorrect usage")
        derived_key = hashlib.pbkdf2_hmac(
            hash_algorithm,
            password,
            salt,
            iterations,
            dklen=key_length
        )
        return derived_key

if __name__ == '__main__':
    sys.exit(Main.main())

