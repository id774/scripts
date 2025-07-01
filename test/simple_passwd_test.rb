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
#  - Correct password length and character set with symbols
#  - Correct password length and character set without symbols
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

  it 'generates password with symbols' do

    # This test verifies that the script generates a password of the correct length
    # and that it includes at least one symbol character when symbols are allowed.
    #
    # Due to the randomness of password generation, it's possible that a generated
    # password of length 16 may consist entirely of alphanumeric characters, even
    # when symbols are included in the character set. To account for this, we use
    # a loop that retries password generation until a password of the correct length
    # is produced that also includes at least one of the expected symbol characters.
    #
    # The following expectations are enforced:
    # - The script exits successfully with status code 0.
    # - The output string has exactly 16 characters.
    # - All characters are from the allowed set: [0-9a-zA-Z_!#&-]
    # - At least one character is a symbol: one of [!#&_\\-]
    #
    # This loop prevents false test failures caused by statistically rare but
    # valid outputs (e.g., a 16-character password that contains no symbols).

    loop do
      output = `ruby #{script_path} 16`.strip
      expect($?.exitstatus).to eq(0)
      next unless output.length == 16
      expect(output).to match(/\A[0-9a-zA-Z_!#&-]{16}\z/)
      expect(output).to match(/[!#&_~-]/)
      break
    end
  end

  it 'generates password without symbols' do
    output = `ruby #{script_path} -s 20`.strip
    expect($?.exitstatus).to eq(0)
    expect(output.length).to eq(20)
    expect(output).to match(/\A[a-zA-Z0-9]{20}\z/)
  end
end

RSpec.configure do |config|
  config.color = true
  config.formatter = :progress
end
