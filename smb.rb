#!/usr/bin/env ruby
$:.unshift File.join(File.dirname(__FILE__), 'lib')

class ExecOnWin
  def run(mount, share, ipaddr, user, pass)
    bs = "\\"
    target = bs + bs + ipaddr + bs + share
    if system("ping -n 1 -w 3 #{ipaddr} > nul")
      puts `net use #{mount} #{target} #{pass} /USER:#{user} /persistent:no`
    else
      puts "#{ipaddr} ping timeout."
    end
  end
end

class ExecOnPosix
  def run(mount, share, ipaddr, user, pass)
    if system("ping -c 1 -i 3 #{ipaddr} > /dev/null")
      puts `sudo smbmount //#{ipaddr}/#{share} #{mount} -o rw,uid=#{user},username=#{user},password=#{pass},iocharset=utf8`
    else
      puts "#{ipaddr} ping timeout."
    end
  end
end

if __FILE__ ==$0
  require 'optparse'
  mount = '~/mnt'
  share = 'homes'
  ipaddr = ''
  user = ''
  pass = ''

  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} [options]"
    parser.separator "options:"
    parser.on('-m', '--mount DIR/DRV', String, "mount (default = ~/mnt)"){|m| mount = m }
    parser.on('-s', '--share DIR', String, "share (default = homes)"){|s| share = s }
    parser.on('-i', '--ipaddr IP', String, "IP address"){|i| ipaddr = i }
    parser.on('-u', '--user username', String, "username"){|u| user = u }
    parser.on('-p', '--pass password', String, "password"){|p| pass = p }
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

  if ARGV.size >= 0 && !ipaddr.empty?
    if ENV['windir'].nil?
      x = ExecOnPosix.new
    else
      x = ExecOnWin.new
    end
    x.run(mount, share, ipaddr, user, pass)
  else
    puts parser.help
  end
end
