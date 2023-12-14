#!/usr/bin/env ruby

########################################################################
# namecalc.rb: Numerology Calculation Script
#
#  Description:
#  This Ruby script performs numerology calculations based on input strings.
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
#  v1.1 2023-12-14
#       Refactored namecalc.rb for improved code readability and maintainability,
#       adapting the structure and style from the Python version while keeping
#       the original functionality and output intact.
#  v1.0 2013-04-12
#       Initial release. Implemented basic numerology calculation and
#       graphical representation.
#
#  Usage:
#  Run the script with one or more strings:
#      ./namecalc.rb [string1 string2 ...]
#
#  Example:
#      ./namecalc.rb 111 153 111 115
#  This will perform numerology calculations on the provided strings and
#  display the results.
#
########################################################################

class NameCalc
  class << self
    def calc(parse_string)
      base_array = split_array(parse_string)
      draw_graph(base_array)
    end

    def number0_5?(str)
      str.match(/[0-5]/)
    end

    def split_array(parse_string)
      base_array = []
      split_char = parse_string.chars
      split_char.each do |char|
        base_array << char if number0_5?(char)
      end
      base_array
    end

    def print_header(header_count)
      print ' ' * header_count
    end

    def cross_sum(base_array, calc_array)
      i = 1
      base_array.each do |e|
        break if i > base_array.length

        print " #{e}"
        n = i < base_array.length ? e.to_i + base_array[i].to_i : 0
        n -= 10 if n >= 10
        calc_array << n
        i += 1
      end
      calc_array
    end

    def draw_graph(base_array)
      parsed_string_size = base_array.size
      while base_array.size >= 2
        calc_array = []
        print_header(parsed_string_size - base_array.size)
        calc_array = cross_sum(base_array, calc_array)
        puts
        base_array = calc_array.dup
        base_array.pop
      end
    end
  end
end

def main
  if ARGV.size > 1
    NameCalc.calc(ARGV.join)
  else
    puts "Usage: #{$0} [string1 string2 ...]"
    puts "Example: #{$0} 111 153 111 115"
    exit 1
  end
end

main if __FILE__ == $0
