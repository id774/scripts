#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), 'lib')

class Del_IRCLogs
  def delete_files(delfiles)
    Dir::glob(delfiles).each do |f|
      if (FileTest.file?(f))
        puts "#{f}: #{File::stat(f).size}bytes"
        File.unlink("#{f}")
      end
    end
  end

  def del_irclogs(deldir, monthcount)
    require 'date'
    day = Date::today
    day = day << monthcount
    year = day.year.to_s
    if (day.month <= 9)
      month = "0" + day.month.to_s
    else
      month = day.month.to_s
    end
    delfiles = deldir + "/**/*" + year + month + ".log"
    delete_files(delfiles)
    puts (delfiles + " has been deleted.")
  end
end

class Logger
  def log_time(messages)
    puts Time.now.strftime("%Y/%m/%d-%X " + messages)
  end
end

if __FILE__ ==$0
  require 'optparse'
  require 'message_logger'

  month = 1

  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} [month] delete_dir
    ex.: #{File.basename($0,".*")} -m 1 C:/var/log/IRC"
    parser.separator "options:"
    parser.on('-m', '--month N', Integer, "delete by the month ago (default 1 month)"){|n| month = n }
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
    d = Del_IRCLogs.new
    d.del_irclogs(ARGV[0], month)
    MessageLogger.msg("program ended.")
  else
    puts parser.help
  end
end
