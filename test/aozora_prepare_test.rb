#!/usr/bin/env ruby

########################################################################
# aozora_prepare_test.rb: Test for aozora_prepare.rb
#
#  Description:
#  This script tests aozora_prepare.rb, which processes Aozora Bunko
#  text files for improved readability. It verifies encoding conversion,
#  ruby annotation removal, full-width space replacement, and newline normalization.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Shows usage and exits with code 0 when invoked with -h option
#    - Successfully converts Aozora Bunko formatted text with expected transformations.
#    - Displays usage and exits with code 0 when arguments are missing.
#
#  Version History:
#  v1.0 2025-07-07
#       Initial release.
#
########################################################################

require 'rspec'
require 'fileutils'

RSpec.describe 'aozora_prepare.rb' do
  let(:script_path) { File.expand_path('../../aozora_prepare.rb', __FILE__) }
  let(:input_path)  { 'test_input.txt' }
  let(:output_path) { 'test_output.txt' }

  before do
    File.open(input_path, 'w:Windows-31J') do |f|
      f.puts "これは《ルビ》テストです。\r\n全角スペース　を含みます。\r\n"
    end
  end

  after do
    FileUtils.rm_f(input_path)
    FileUtils.rm_f(output_path)
  end

  it 'shows usage when -h option is given' do
    output = `ruby #{script_path} -h`
    expect($?.exitstatus).to eq(0)
    expect(output).to include('Usage')
  end

  it 'processes Aozora text and outputs normalized result' do
    output = `ruby #{script_path} #{input_path} #{output_path}`
    expect($?.exitstatus).to eq(0)
    expect(File).to exist(output_path)

    result = File.read(output_path, encoding: 'UTF-8')
    expect(result).to include("これはテストです。\n")
    expect(result).to include("全角スペース  を含みます。\n")
    expect(result).not_to include("《ルビ》")
    expect(result).not_to include("\r\n")
  end

  it 'displays usage when arguments are missing' do
    output = `ruby #{script_path} 2>&1`
    expect($?.exitstatus).to eq(0)
    expect(output).to include("aozora_prepare.rb")
    expect(output).to include("Usage")
  end
end
