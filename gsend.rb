#!/usr/bin/env ruby
$:.unshift File.join(File.dirname(__FILE__), 'lib')

if __FILE__ ==$0
  require 'gmail_sender'
  require 'optparse'

  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} passwordfile mailfile"
    parser.separator "options:"
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

  if ARGV.size >= 2
    g = GmailSender.new
    g.send(ARGV[0], ARGV[1])
  else
    puts parser.help
  end
end
