#!/opt/ruby/1.9.3/bin/ruby

require 'zipruby'
require 'tempfile'
require 'sysadmin'

@total = 0

def lineno(path)
  line = open(path).each{}.lineno
  @total += line
  return line
end

def lineno_zip(src)
  Zip::Archive.open(src) do |a|
    a.each do |f|
      tempfile = Tempfile::new(f.name)
      tempfile.print(f.read)
      puts "#{lineno(tempfile.path)}\t#{f.name}"
    end
  end
end

def wc(subdir=true)
  subdir = false unless ARGV[1].nil?
  Dir.filelist(ARGV[0], subdir).each {|f|
    if f =~ /\.zip\Z/
      lineno_zip(f)
    else
      puts "#{lineno(f)}\t#{f}"
    end
  }
  puts "#{@total}\t#{ARGV[0]}\t[Total]"
end

if __FILE__ == $0
  unless ARGV[0].nil?
    wc
  else
    puts "Syntax: wc4zip zipdir"
  end
end
