#!/usr/bin/env ruby
$:.unshift File.join(File.dirname(__FILE__), 'lib')

class ExecOnPosix
  def run(repo, message, action)
    case action
    when 'push'
      puts `svk ci -m #{message}`
      puts `svk sm -m #{message} //#{repo} //mirror//#{repo}`
    when 'smerge'
      puts `svk sm -m #{message} //#{repo} //mirror//#{repo}`
    else
      puts `svk ci -m #{message}`
    end
  end
end

class ExecOnWin
  def run(repo, message, action)
    case action
    when 'push'
      puts `call svk ci -m #{message}`
      puts `call svk sm -m #{message} //#{repo} //mirror//#{repo}`
    when 'smerge'
      puts `call svk sm -m #{message} //#{repo} //mirror//#{repo}`
    else
      puts `call svk ci -m #{message}`
    end
  end
end

if __FILE__ ==$0
  require 'optparse'
  action = 'commit'

  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} - Simple SVK - by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} [options] RepositoryName Message"
    parser.separator "options:"
    parser.on('-c', '--commit', "commit"){ action = 'commit' }
    parser.on('-s', '--smerge', "smart merge"){ action = 'smerge' }
    parser.on('-p', '--push', "push"){ action = 'push' }
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

  if ARGV.size >= 2
    if ENV['windir'].nil?
      x = ExecOnPosix.new
    else
      x = ExecOnWin.new
    end
    x.run(ARGV[0], ARGV[1], action)
  else
    puts parser.help
  end
end
