#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
#
########################################################################
# Prepare for Aozora Bunko
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 1/22,2014
#       Stable.
########################################################################

class Aozora
  def initialize(args)
    @infile  = args.shift || "in.txt"
    @outfile = args.shift || "out.txt"
  end

  def run
    open(@infile, "r:Windows-31J:UTF-8") {|source|
      open(@outfile, "w") {|data|
        s = source.read
        s = s.gsub(/《[^》]+》/, "")
        s = s.gsub(/　/, "  ")
        data.print s.gsub(/(\r\n)/, "\n")
      }
    }
  end
end

if __FILE__ == $0
  if ARGV.length == 2
    aozora = Aozora.new(ARGV)
    aozora.run
  else
    puts "Syntax: aozora_prepare.rb [infile] [outfile]"
  end
end
