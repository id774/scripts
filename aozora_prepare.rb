#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

def run(infile, outfile)
  open(infile, "r:Windows-31J:UTF-8") {|source|
    open(outfile, "w") {|data|
      s = source.read
      s = s.gsub(/《[^》]+》/, "")
      s = s.gsub(/　/, "  ")
      data.print s.gsub(/(\r\n)/, "\n")
    }
  }
end

if __FILE__ == $0
  run(ARGV[0],ARGV[1])
end
