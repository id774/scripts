#!/usr/bin/env ruby

########################################################################
# simple_passwd_test.rb: Test suite for simple_passwd.rb
#
#  Description:
#  This test suite verifies the behavior of the simple_passwd.rb script.
#  It checks input validation and password generation logic.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-06-24
#      Initial release. Focused tests on input handling and password generation.
#
#  Dependencies:
#  This test script requires the RSpec library. To install RSpec, run:
#  `gem install rspec`
#
#  Running the tests:
#  Execute the test script from the command line:
#  `rspec test/simple_passwd_test.rb`
#
#  Test Cases:
#  - Exit when no arguments are given
#  - Error on non-numeric length argument
#  - Correct password length and character set with symbols
#  - Correct password length and character set without symbols
#
########################################################################

require 'rspec'
require 'stringio'

describe 'simple_passwd.rb' do
  let(:script_path) { File.expand_path('../../simple_passwd.rb', __FILE__) }

  before(:each) do
    @original_argv = ARGV.dup
    @original_stdout = $stdout
    $stdout = StringIO.new
  end

  after(:each) do
    ARGV.replace(@original_argv)
    $stdout = @original_stdout
  end

  it 'shows usage when no arguments are given' do
    ARGV.clear
    expect { load script_path }.to raise_error(SystemExit)
    output = $stdout.string
    expect(output).to include('Simple Password Generator in Ruby')
  end

  it 'prints error when length is not a number' do
    ARGV.replace(['abc'])
    expect { load script_path }.to raise_error(SystemExit)
    output = $stdout.string
    expect(output).to include('[ERROR] Length must be a number.')
  end

  it 'generates password with symbols' do
    ARGV.replace(['16'])
    loop do
      $stdout = StringIO.new
      load script_path
      output = $stdout.string.strip
      next unless output.length == 16
      expect(output).to match(/\A[0-9a-zA-Z_\-!#&]{16}\z/)
      break
    end
  end

  it 'generates password without symbols' do
    ARGV.replace(['-s', '20'])
    $stdout = StringIO.new
    load script_path
    output = $stdout.string.strip
    expect(output.length).to eq(20)
    expect(output).to match(/\A[a-zA-Z0-9]{20}\z/)
  end
end

RSpec.configure do |config|
  config.color = true
  config.formatter = :progress
end
