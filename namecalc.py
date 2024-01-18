#!/usr/bin/env python

########################################################################
# namecalc.py: Numerology Calculation Script
#
#  Description:
#  This Python script performs numerology calculations based on input strings.
#  It translates each character of the string into a numerical value and
#  performs calculations to output a graphical representation of the
#  numerological analysis. This can be used for names or any other strings.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2023-12-13
#       Python adaptation from Ruby.
#       Initial release. Implemented basic numerology calculation and
#       graphical representation.
#
#  Usage:
#  Run the script with one or more strings:
#      ./namecalc.py [string1 string2 ...]
#
#  Example:
#      ./namecalc.py 111 153 111 115
#  This will perform numerology calculations on the provided strings and
#  display the results.
#
########################################################################

import re
import sys


class NameCalc:
    @staticmethod
    def calc(parse_string):
        base_array = NameCalc.split_array(parse_string)
        NameCalc.draw_graph(base_array)

    @staticmethod
    def number0_5(str):
        return re.search(r'[0-5]', str) is not None

    @staticmethod
    def split_array(parse_string):
        base_array = []
        split_char = list(parse_string)
        for char in split_char:
            if NameCalc.number0_5(char):
                base_array.append(char)
        return base_array

    @staticmethod
    def print_header(header_count):
        for _ in range(header_count):
            print(" ", end="")

    @staticmethod
    def cross_sum(base_array, calc_array):
        i = 1
        for e in base_array:
            if i <= len(base_array):
                print(" %s" % e, end="")
                n = int(e) + int(base_array[i]) if i < len(base_array) else 0
                if n >= 10:
                    n -= 10
                calc_array.append(n)
                i += 1
        return calc_array

    @staticmethod
    def draw_graph(base_array):
        calc_array = []
        parsed_string_size = len(base_array)
        while len(base_array) >= 2:
            calc_array = []
            NameCalc.print_header(parsed_string_size - len(base_array))
            calc_array = NameCalc.cross_sum(base_array, calc_array)
            print("\n", end="")
            base_array = calc_array[:]
            base_array.pop()


def main():
    if len(sys.argv) > 1:
        NameCalc.calc("".join(sys.argv[1:]))
    else:
        print("Usage: {} [string1 string2 ...]".format(sys.argv[0]))
        print("Example: {} 111 153 111 115".format(sys.argv[0]))
        sys.exit(1)


if __name__ == "__main__":
    main()
