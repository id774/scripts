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
      puts "#{lineno(tempfile.path)} #{f.name}"
    end
  end
end

def run(subdir=false)
  subdir = true unless ARGV[1].nil?
  Dir.filelist(ARGV[0], subdir).each {|f|
    if f =~ /\.zip\Z/
      lineno_zip(f)
    else
      puts "#{lineno(f)} #{f}"
    end
  }
  puts "#{@total} Total"
end

if __FILE__ == $0
  unless ARGV[0].nil?
    run
  else
    puts "Syntax: wc4zip zipdir"
  end
end
