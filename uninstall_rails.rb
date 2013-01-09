#!/usr/bin/env ruby

def uninstall_rails(version)
  prefix = "gem uninstall -v #{version} "
  system(prefix + "rails")
  system(prefix + "railties")
  system(prefix + "actionmailer")
  system(prefix + "actionpack")
  system(prefix + "activerecord")
  system(prefix + "activeresource")
  system(prefix + "activemodel")
  system(prefix + "activesupport")
end

if __FILE__ == $0
  version=ARGV.shift || "3.2.10"
  uninstall_rails(version)
end
