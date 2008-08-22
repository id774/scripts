#!/usr/bin/env ruby

class RAphro
  def number0_5?(str)
    /[0-5]/ =~ str
  end

  def raphro(parse_string)
    split_char = parse_string.scan(/.{1,1}/m)
    i = 0
    base_array = Array.new
    while i < parse_string.size do
      if number0_5?(split_char[i])
        base_array << split_char[i]
      end
      i += 1
    end
    calc_array = Array.new

    parsed_string_size = base_array.size

    while base_array.size >= 2 do
      calc_array.clear
      (parsed_string_size - base_array.size).times do
        $stdout.printf(" ")
      end
      i = 1
      base_array.each{|elem|
      if (i <= base_array.length)
        $stdout.printf(" %s", elem)
        num = elem.to_i + base_array[i].to_i
        if num >= 10
          num = num - 10
        end
        calc_array << num
        i += 1
      end
      }
      $stdout.printf("\n")
      base_array.replace(calc_array)
      base_array.pop
    end
  end
end

if __FILE__ ==$0
  require 'optparse'

  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} parse_strings
    ex.: #{File.basename($0,".*")} 111 153 111 115 (YAMADA TAROU YAMADA HANAKO)"
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

  if ARGV.size >= 1
    r = RAphro.new
    r.raphro(ARGV.join)
  else
    puts parser.help
  end
end
