#!/usr/bin/env ruby

########################################################################
# aozora_prepare.rb: Prepare for Aozora Bunko Text Processing
#
#  Description:
#  This script processes text files from Aozora Bunko for readability and
#  standardization. It converts the encoding, removes annotations, replaces
#  full-width spaces, and standardizes newlines.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Refactored for improved readability and documentation.
#  v1.0 2014-01-22
#       Initial release.
#
#  Usage:
#  ./aozora_prepare.rb [input file] [output file]
#
########################################################################

class Aozora
  def initialize(args)
    @infile  = args.shift || "in.txt"
    @outfile = args.shift || "out.txt"
  end

  def run
    File.open(@infile, "r:Windows-31J:UTF-8") do |source|
      File.open(@outfile, "w") do |data|
        content = source.read
        content.gsub!(/《[^》]+》/, "")
        content.gsub!(/　/, "  ")
        data.print content.gsub(/(\r\n)/, "\n")
      end
    end
  end
end

if __FILE__ == $0
  if ARGV.length == 2
    Aozora.new(ARGV).run
  else
    puts "Syntax: aozora_prepare.rb [infile] [outfile]"
  end
end

