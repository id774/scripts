#!/usr/bin/env ruby
$:.unshift File.join(File.dirname(__FILE__), 'lib')

require 'twitter_post'
user = 'twitt'
pass = 'xxxxxxxx'
status = ARGV.join(" ") || ""
t = TwitterPost.new
t.post(user, pass, status)
