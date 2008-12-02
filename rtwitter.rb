#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), "lib")

if __FILE__ ==$0
  require 'twitter_post'
  require 'optparse'

  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} username password status"
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

  if ARGV.size >= 3
    require 'kconv'
    user = ARGV.shift
    pass = ARGV.shift
    status = ARGV.join(" ") || ""
    status_utf8 = status.kconv(Kconv::UTF8, Kconv::SJIS);
    status_utf8 += ""
    t = TwitterPost.new
    t.post(user, pass, status_utf8)
  else
    puts parser.help
  end
end
