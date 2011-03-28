#!/usr/bin/env ruby

class NameCalc
end

class << NameCalc
  def number0_5?(str)
    /[0-5]/ =~ str
  end

  def split_base_array(parse_string)
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

  def sum_up_base_array(base_array, calc_array)
    i = 1
    base_array.each{|e|
    if (i <= base_array.length)
      $stdout.printf(" %s", e)
      n = e.to_i + base_array[i].to_i
      if n >= 10
        n = n - 10
      end
      calc_array << n
      i += 1
    end
    }
    calc_array
  end

  def print_header_space(header_count)
    header_count.times do
      $stdout.printf(" ")
    end
  end

  def print_graph(base_array)
    calc_array = Array.new
    parsed_string_size = base_array.size
    while base_array.size >= 2 do
      calc_array.clear
      print_header_space(parsed_string_size - base_array.size)
      calc_array = sum_up_base_array(base_array, calc_array)
      $stdout.printf("\n")
      base_array.replace(calc_array)
      base_array.pop
    end
  end

  def calc(parse_string)
    base_array = split_base_array(parse_string)
    print_graph(base_array)
  end
end

def main
  require 'optparse'
  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} parse_strings
    ex.: #{File.basename($0,".*")} 111 153 111 115 (YAMADA TAROU YAMADA HANAKO)"
    parser.separator "options:"
    parser.on('-h', '--help', "show this message") {
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
    NameCalc.calc(ARGV.join)
  else
    puts parser.help
  end
end

if __FILE__ ==$0
  main
end
