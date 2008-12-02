#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), 'lib')

require 'twitter_post'
user = 'twitt_'
pass = 'xxxxxxxx'
status = ARGV.join(" ") || ""
t = TwitterPost.new
t.post(user, pass, status)
