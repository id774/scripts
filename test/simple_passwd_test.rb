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
#  v1.2 2025-07-03
#      Updated tests to reflect new rule: symbols are always included if enabled.
#  v1.1 2025-07-01
#      Updated for function-based execution and explicit exit codes.
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
#  - Error on zero length argument
#  - Password with symbols includes at least one symbol
#  - Password without symbols excludes all symbols
#  - Single-character password with symbols is symbol-only
#
########################################################################

require 'rspec'

describe 'simple_passwd.rb' do
  let(:script_path) { File.expand_path('../../simple_passwd.rb', __FILE__) }

  it 'shows usage when no arguments are given' do
    output = `ruby #{script_path}`
    expect($?.exitstatus).to eq(0)
    expect(output).to include('Simple Password Generator in Ruby')
  end

  it 'prints error and exits with 1 when length is not a number' do
    output = `ruby #{script_path} abc 2>&1`
    expect($?.exitstatus).to eq(1)
    expect(output).to include('[ERROR] Length must be a number.')
  end

  it 'prints error and exits with 1 when length is zero' do
    output = `ruby #{script_path} 0 2>&1`
    expect($?.exitstatus).to eq(1)
    expect(output).to include('[ERROR] Length must be greater than zero.')
  end

  it 'generates password with symbols (at least one symbol)' do
    output = `ruby #{script_path} 16`.strip
    expect($?.exitstatus).to eq(0)
    expect(output.length).to eq(16)
    expect(output).to match(/\A[0-9a-zA-Z_!#&-]{16}\z/)
    expect(output).to match(/[!#&_~-]/)
  end

  it 'generates password without symbols (all alphanumeric)' do
    output = `ruby #{script_path} -s 20`.strip
    expect($?.exitstatus).to eq(0)
    expect(output.length).to eq(20)
    expect(output).to match(/\A[a-zA-Z0-9]{20}\z/)
  end

  it 'generates one-character password with symbol when length is 1 and symbols enabled' do
    output = `ruby #{script_path} 1`.strip
    expect($?.exitstatus).to eq(0)
    expect(output.length).to eq(1)
    expect(output).to match(/\A[!#&_~-]\z/)
  end
end

RSpec.configure do |config|
  config.color = true
  config.formatter = :progress
end
