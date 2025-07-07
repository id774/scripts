#!/usr/bin/env ruby

########################################################################
#  convert_msime2canna_test.rb: Test for convert_msime2canna.rb
#
#  Description:
#  This script tests convert_msime2canna.rb, which converts a Microsoft
#  IME dictionary into a format suitable for Canna. It checks for correct
#  conversion of emoji entries and proper usage output.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-07-07
#       Initial release.
#
#  Test Cases:
#  - Shows usage and exits with code 0 when invoked with -h option
#  - Converts "顔文字" entry into Canna format with escaped space
#  - Skips non-emoji entries as expected
#
########################################################################

require 'rspec'
require 'tempfile'

RSpec.describe 'convert_msime2canna.rb' do
  let(:script_path) { File.expand_path('../../convert_msime2canna.rb', __FILE__) }

  def run_script_with_input(input)
    IO.popen("ruby #{script_path}", 'r+') do |io|
      io.write(input)
      io.close_write
      io.read
    end
  end

  it 'shows usage when -h option is given' do
    output = `ruby #{script_path} -h`
    expect($?.exitstatus).to eq(0)
    expect(output).to include('Usage')
  end

  it 'converts MS-IME emoji entry to Canna format' do
    input = "かお\t(´・ω・`)\t顔文字\n"
    output = run_script_with_input(input)
    expect(output).to eq("かお #KJ (´・ω・`)\n")
  end

  it 'escapes spaces in emoji entry' do
    input = "にこ\t(* ^_^)人(^_^ *)\t顔文字\n"
    output = run_script_with_input(input)
    expect(output).to eq("にこ #KJ (*\\ ^_^)人(^_^\\ *)\n")
  end

  it 'ignores entries that are not emoji' do
    input = "りんご\tapple\t名詞\n"
    output = run_script_with_input(input)
    expect(output).to eq("")
  end
end
