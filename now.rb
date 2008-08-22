#!/usr/bin/env ruby
#
# == Synopsis
#
# Display the current date and time.
#
# == Usage
#
# now [ -h | --help ] [ -f | --fmt fmtstring ]
#
# == Author
#
# id774
#
# == Copyright
#
# Copyright (c) id774 <idnanashi@gmail.com>
# Licensed under the same terms as Ruby.
#

require 'optparse'
require 'rdoc/usage'

fmt = "%Y/%m/%d %X %a"

opts = OptionParser.new
opts.on("-h", "--help") { RDoc::usage }
opts.on("-f", "--fmt FMTSTRING") {|str| fmt = str}
opts.parse(ARGV) rescue RDoc::usage('usage')

puts Time.now.strftime(fmt)
