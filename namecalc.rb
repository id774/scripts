#!/usr/bin/env ruby

class NameCalc
end

class << NameCalc
  def calc(parse_string)
    base_array = split_array(parse_string)
    draw_graph(base_array)
  end

  private
  def number0_5?(str)
    /[0-5]/ =~ str
  end

  def print_header(header_count)
    header_count.times do
      $stdout.printf(" ")
    end
  end

  def split_array(parse_string)
    base_array = Array.new
    split_char = parse_string.scan(/.{1,1}/m)
    i = 0
    while i < parse_string.size do
      if number0_5?(split_char[i])
        base_array << split_char[i]
      end
      i += 1
    end
    base_array
  end

  def cross_sum(base_array, calc_array)
    i = 1
    base_array.each do |e|
      if (i <= base_array.length)
        $stdout.printf(" %s", e)
        n = e.to_i + base_array[i].to_i
        if n >= 10
          n = n - 10
        end
        calc_array << n
        i += 1
      end
    end
    calc_array
  end

  def draw_graph(base_array)
    calc_array = Array.new
    parsed_string_size = base_array.size
    while base_array.size >= 2 do
      calc_array.clear
      print_header(parsed_string_size - base_array.size)
      calc_array = cross_sum(base_array, calc_array)
      $stdout.printf("\n")
      base_array.replace(calc_array)
      base_array.pop
    end
  end
end

def main
  require 'optparse'
  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} #{version} by #{author}
    Usage: #{File.basename($0,".*")} parse_strings
    ex.: #{File.basename($0,".*")} 111 153 111 115 (YAMADA TAROU YAMADA HANAKO)"
    parser.separator "options:"
    parser.on('-h', '--help', "show this message") {
      puts parser
      exit 1
    }
  end

  begin
    parser.parse!
  rescue OptionParser::ParseError => err
    $stderr.puts err.message
    $stderr.puts parser.help
    exit 2
  end

  if ARGV.size >= 1
    NameCalc.calc(ARGV.join)
  else
    puts parser.help
    exit 1
  end
end

def author
  "id774 <774@id774.net>"
end

def version
  "1.0"
end

if __FILE__ ==$0
  main
end
