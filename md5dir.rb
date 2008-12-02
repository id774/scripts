#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), 'lib')

require 'dirglob'
require 'md5sum'

class MD5Dir
  def md5puts(name)
    f = File.open(name,"rb")
    t = File.stat(name)
    s = MD5Sum.new
    puts "MD5:#{s.md5sum(f)} #{t.mtime.strftime("%Y/%m/%d-%X")} #{t.size} #{name}"
    f.close
  end

  def dir(argv0, sub_directory)
    DirGlob.dir(argv0, sub_directory).each do |f|
      md5puts(f)
    end
  end
end

if __FILE__ ==$0
  require 'optparse'
  sub_directory = false

  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} [path]"
    parser.separator "options:"
    parser.on('-d', '--subdirectory', "include sub directory"){ sub_directory = true }
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

  m = MD5Dir.new
  if ARGV.size >= 1
    m.dir(ARGV[0], sub_directory)
  else
    m.dir(".", sub_directory)
  end
end
