#!/usr/bin/env ruby

require 'zipruby'
require 'tempfile'
require 'sysadmin'

@total_lines = 0
@total_size = 0

def get_linesize(path)
  line = open(path).each{}.lineno
  @total_lines += line
  return line
end

def get_filesize(path)
  size = File.size(path)
  @total_size += size
  return size
end

def unzip(src)
  Zip::Archive.open(src) do |a|
    a.each do |f|
      tempfile = Tempfile::new(f.name)
      tempfile.print(f.read)
      puts "#{get_linesize(tempfile.path)}\t#{get_filesize(tempfile.path)}\t#{f.name}"
      tempfile.close
    end
  end
end

def wc(subdir=true)
  subdir = false unless ARGV[1].nil?
  Dir.filelist(ARGV[0], subdir).each {|f|
    if f =~ /\.zip\Z/
      unzip(f)
    else
      puts "#{get_linesize(f)}\t#{get_filesize(f)}\t#{f}"
    end
  }
  puts "#{@total_lines}\t#{@total_size}\t#{ARGV[0]}\t[Total]"
end

if __FILE__ == $0
  unless ARGV[0].nil?
    wc
  else
    puts "Syntax: wc4zip zipdir"
  end
end
