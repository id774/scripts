#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), 'lib')

require 'optparse'
require 'rubycipher'

if __FILE__ ==$0
  action = ''
  ciphertype = 'aes256'
  key = ''

  parser = OptionParser.new do |parser|
    parser.banner = "#{File.basename($0,".*")} by id774 <idnanashi@gmail.com>
    Usage: #{File.basename($0,".*")} [input] [output]"
    parser.separator "options:"
    parser.on('-e', '--encrypt', "encrypt"){ action = 'encrypt' }
    parser.on('-d', '--decrypt', "decrypt"){ action = 'decrypt' }
    parser.on('-c', '--ciphertype TYPE', String, "ciphertype (default = aes256)"){|c| ciphertype = c }
    parser.on('-k', '--key KEY', String, "key"){|k| key = k }
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
    c = RubyCipher.new
    c.cipher(ARGV[0], ARGV[1], action, ciphertype, key)
  else
    puts parser.help
  end
end
