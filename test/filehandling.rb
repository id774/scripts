#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), '..', 'lib')

require 'file_string_handler'

testfile_1 = 'hoge.txt'
testfile_2 = 'fuga.txt'
src        = 'hoge'
out        = 'fuga'
exclude    = 'hoge'

FileStringHandler.appendString(testfile_1,src)
FileStringHandler.appendString(testfile_2,src)
FileStringHandler.replaceFile(testfile_2,src,out)
FileStringHandler.deleteString(testfile_1,exclude)
