#!/usr/bin/env python

########################################################################
# md5.py: MD5 Checksum Calculator
#
#  Description:
#  This script calculates MD5 checksums for files and strings. It can process
#  individual files, directories, and input strings, and supports various output
#  formats including a quiet mode that only displays checksums.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.0 2023-06-24
#       Initial release. Functionality for calculating MD5 checksums of files and
#       strings, supporting subdirectories, reverse output format, and quiet mode.
#
#  Usage:
#  Calculate checksum for files:
#      python md5.py [options] file
#
#  Calculate checksum for a string:
#      python md5.py -s "string"
#
#  Options:
#   -v, --version: Show the version of the script and exit.
#   -d, --subdirectory: Include subdirectories when calculating checksums for files.
#   -r, --reversed: Reverse the format of the output (checksum first, then file path).
#   -q, --quiet: Quiet mode, only the checksum is printed.
#   -p, --print: Echo stdin to stdout and append the checksum to stdout (useful for piping).
#   -s, --string: Calculate and print the checksum of the given string.
#
#  Example:
#  Calculate checksum for all files in a directory (including subdirectories):
#      python md5.py -d /path/to/directory
#
#  Calculate checksum for a single file in quiet mode:
#      python md5.py -q /path/to/file
#
#  Calculate and print checksum for stdin input:
#      echo "sample text" | python md5.py -p
#
########################################################################

import hashlib
import os
import sys
from optparse import OptionParser
from stat import S_ISDIR, ST_MODE


class Md5Checksum:

    @staticmethod
    def calculate_checksum(path):
        """ Calculate and return the MD5 checksum for a given file. """
        m = hashlib.md5()
        with open(path, 'rb') as f:
            for line in f:
                m.update(line)
        return m.hexdigest()

    @staticmethod
    def calculate_checksum_for_string(input_string):
        """ Calculate and return the MD5 checksum for a given input string. """
        m = hashlib.md5()
        m.update(input_string.encode('utf-8'))
        return m.hexdigest()

def print_checksum(include_subdir, reversed_format, quiet_mode, paths):
    """ Calculate and print MD5 checksums for given paths with optional inclusion of subdirectories. """
    script_name = os.path.basename(sys.argv[0])
    for path in paths:
        if not os.path.exists(path):
            print("[WARN] {0}: {1}: No such file or directory".format(script_name, path), file=sys.stderr)
            continue
        if include_subdir and S_ISDIR(os.stat(path)[ST_MODE]):
            for root, dirs, files in os.walk(path):
                for file in files:
                    full_path = os.path.join(root, file)
                    checksum = Md5Checksum.calculate_checksum(full_path)
                    print_formatted_checksum(
                        checksum, full_path, reversed_format, quiet_mode, is_file=True)
        elif S_ISDIR(os.stat(path)[ST_MODE]):
            print("[WARN] {0}: {1}: Is a directory".format(script_name, path), file=sys.stderr)
        else:
            checksum = Md5Checksum.calculate_checksum(path)
            print_formatted_checksum(
                checksum, path, reversed_format, quiet_mode, is_file=True)

def print_formatted_checksum(checksum, path, reversed_format, quiet_mode, is_file):
    """ Print the MD5 checksum in a formatted manner based on the provided options. """
    if quiet_mode:
        print(checksum)
    elif reversed_format:
        print("{0} {1}".format(checksum, path if is_file else '"{}"'.format(path)))
    else:
        print("MD5 ({0}) = {1}".format(
            path if is_file else '"{}"'.format(path), checksum))

def main():
    """ Parse command-line arguments and execute the MD5 checksum calculation and printing. """
    usage = "usage: %prog [options] file"
    parser = OptionParser(usage)
    parser.add_option("-v", "--version", help="show the version and exit",
                      action="store_true", dest="version")
    parser.add_option("-d", "--subdirectory", help="include sub directory",
                      action="store_true", dest="include_subdir")
    parser.add_option("-r", "--reversed", help="reverses the format of the output",
                      action="store_true", dest="reversed_format")
    parser.add_option("-q", "--quiet", help="quiet mode - only the checksum is printed out",
                      action="store_true", dest="quiet_mode")
    parser.add_option("-p", "--print", help="echo stdin to stdout and append the checksum to stdout",
                      action="store_true", dest="print_stdin")
    parser.add_option("-s", "--string", help="print a checksum of the given string",
                      action="store", type="string", dest="input_string")
    (options, args) = parser.parse_args()
    if options.version:
        print("{0} Version 1.0 Copyright (c) 2023 id774 <idnanashi@gmail.com>".format(
            os.path.basename(sys.argv[0])))
    elif options.input_string:
        checksum = Md5Checksum.calculate_checksum_for_string(
            options.input_string)
        print_formatted_checksum(checksum, options.input_string,
                                 options.reversed_format, options.quiet_mode, is_file=False)
    elif options.print_stdin:
        input_data = sys.stdin.read()
        print(input_data, end='')
        checksum = Md5Checksum.calculate_checksum_for_string(input_data)
        print("{0}".format(checksum))
    elif len(args) < 1:
        parser.print_help()
    else:
        print_checksum(options.include_subdir,
                       options.reversed_format, options.quiet_mode, args)


if __name__ == '__main__':
    main()
