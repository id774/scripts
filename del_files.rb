#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), 'lib')

if __FILE__ ==$0
  require 'optparse'
  require 'message_logger'
  require 'file_eraser'

  hours = 24

  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} [hours] delete_dir
    ex.: #{File.basename($0,".*")} C:/tmp
    ex.: #{File.basename($0,".*")} -o 720 C:/var/log/IRC"
    parser.separator "options:"
    parser.on('-o', '--old N', Integer, "older than N hours (default 24 hours)"){|n| hours = n }
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
    MessageLogger.msg("program started.")
    e = FileEraser.new
    e.erase(ARGV[0], hours)
    MessageLogger.msg("program ended.")
  else
    puts parser.help
  end
end
