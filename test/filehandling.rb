#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), '..', 'lib')

require 'file_handling'

testfile_1 = 'hoge.txt'
testfile_2 = 'fuga.txt'
src        = 'hoge'
out        = 'fuga'

FileHandling.appendString(testfile_1,src)
FileHandling.appendString(testfile_2,src)
FileHandling.replaceFile(testfile_2,src,out)
