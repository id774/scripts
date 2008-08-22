#!/usr/bin/env ruby
$:.unshift File.join(File.dirname(__FILE__), 'lib')

require 'optparse'

filename = 'None'
verbose = false

parser = OptionParser.new do |parser|
  parser.banner = "#{File.basename($0,".*")}
  Usage: #{File.basename($0,".*")} [options] arg"
  parser.separator "options:"
  parser.on('-f', '--file FILE', String, "read data from FILENAME"){|f| filename = f }
  parser.on('-v', '--verbose', "verbose"){ verbose = true }
  parser.on('-q', '--quiet', "quiet"){ verbose = false }
  parser.on('-h', '--help', "show this message"){
    puts parser
    exit
  }
end

begin
  parser.parse!
rescue OptionParser::ParseError => err
  $stderr.puts err.message
  $stderr.puts parser.help
  exit 1
end

if ARGV.size >= 1
  p filename
  p verbose
  p ARGV
else
  puts parser.help
end
