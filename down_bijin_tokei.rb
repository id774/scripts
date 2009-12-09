#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require 'net/http'
require 'uri'

error_files = ""
0..24.times do |hour|
   0..60.times do |minute|
      now_hour = sprintf("%0#{2}d", hour)
      now_minute = sprintf("%0#{2}d", minute)
      sleep 1
      begin
         Net::HTTP.start("bijint.com", 80) do |http|
         response = http.get("/jp/img/clk/#{now_hour}#{now_minute}.jpg",
                             {"Referer" => "http://bijint.com/jp/" })
         open("#{now_hour}#{now_minute}.jpg", "wb") do |file|
             file.puts response.body
         end
      end
      rescue 
         p "#{now_hour}#{now_minute}.jpg can not get"
         error_files += "http://bijint.com/jp/img/clk/#{now_hour}#{now_minute}.jpg\n"
      end
   end
end
