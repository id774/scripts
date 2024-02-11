#!/usr/bin/env ruby

########################################################################
# namecalc_test.rb: Test suite for Numerology Calculation Script
#
#  Description:
#  This test suite verifies the correctness of the namecalc.rb script.
#  It tests the script's functionality by providing various input strings
#  and comparing the script's output against expected results.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2023-12-13
#      Initial release. Test suite for namecalc.rb script.
#
#  Dependencies:
#  This test script requires the RSpec library. To install RSpec, run:
#  `gem install rspec`
#
#  Running the tests:
#  Execute the test script from the command line:
#  `rspec test/namecalc_test.rb`
#
#  Test Cases:
#  - Verify correct numerological calculation and graphical representation.
#
########################################################################

require 'rspec'
require_relative '../namecalc.rb'

describe NameCalc do
  before(:each) do
    # Redirecting stdout to capture the script output
    @original_stdout = $stdout
    $stdout = StringIO.new
  end

  after(:each) do
    # Resetting stdout
    $stdout = @original_stdout
  end

  def run_test(input, expected_output)
    ARGV.replace input.split
    main
    output = $stdout.string
    expect(output).to eq(expected_output)
  end

  it "calculates and prints the correct numerology graph for '1234512345 5432154321'" do
    input = '1234512345 5432154321'
    expected_output = <<-GRAPH
 1 2 3 4 5 1 2 3 4 5 5 4 3 2 1 5 4 3 2 1
  3 5 7 9 6 3 5 7 9 0 9 7 5 3 6 9 7 5 3
   8 2 6 5 9 8 2 6 9 9 6 2 8 9 5 6 2 8
    0 8 1 4 7 0 8 5 8 5 8 0 7 4 1 8 0
     8 9 5 1 7 8 3 3 3 3 8 7 1 5 9 8
      7 4 6 8 5 1 6 6 6 1 5 8 6 4 7
       1 0 4 3 6 7 2 2 7 6 3 4 0 1
        1 4 7 9 3 9 4 9 3 9 7 4 1
         5 1 6 2 2 3 3 2 2 6 1 5
          6 7 8 4 5 6 5 4 8 7 6
           3 5 2 9 1 1 9 2 5 3
            8 7 1 0 2 0 1 7 8
             5 8 1 2 2 1 8 5
              3 9 3 4 3 9 3
               2 2 7 7 2 2
                4 9 4 9 4
                 3 3 3 3
                  6 6 6
                   2 2
    GRAPH
    run_test(input, expected_output)
  end

  it "calculates and prints the correct numerology graph for '1234512345 1234512345'" do
    input = '1234512345 1234512345'
    expected_output = <<-GRAPH
 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5
  3 5 7 9 6 3 5 7 9 6 3 5 7 9 6 3 5 7 9
   8 2 6 5 9 8 2 6 5 9 8 2 6 5 9 8 2 6
    0 8 1 4 7 0 8 1 4 7 0 8 1 4 7 0 8
     8 9 5 1 7 8 9 5 1 7 8 9 5 1 7 8
      7 4 6 8 5 7 4 6 8 5 7 4 6 8 5
       1 0 4 3 2 1 0 4 3 2 1 0 4 3
        1 4 7 5 3 1 4 7 5 3 1 4 7
         5 1 2 8 4 5 1 2 8 4 5 1
          6 3 0 2 9 6 3 0 2 9 6
           9 3 2 1 5 9 3 2 1 5
            2 5 3 6 4 2 5 3 6
             7 8 9 0 6 7 8 9
              5 7 9 6 3 5 7
               2 6 5 9 8 2
                8 1 4 7 0
                 9 5 1 7
                  4 6 8
                   0 4
    GRAPH
    run_test(input, expected_output)
  end

  it "calculates and prints the correct numerology graph for '111 153 111 115'" do
    input = '111 153 111 115'
    expected_output = <<-GRAPH
 1 1 1 1 5 3 1 1 1 1 1 5
  2 2 2 6 8 4 2 2 2 2 6
   4 4 8 4 2 6 4 4 4 8
    8 2 2 6 8 0 8 8 2
     0 4 8 4 8 8 6 0
      4 2 2 2 6 4 6
       6 4 4 8 0 0
        0 8 2 8 0
         8 0 0 8
          8 0 8
           8 8
    GRAPH
    run_test(input, expected_output)
  end

  it "calculates and prints the correct numerology graph for '1 2 3 4 5 5 4 3 2 1'" do
    input = '1 2 3 4 5 5 4 3 2 1'
    expected_output = <<-GRAPH
 1 2 3 4 5 5 4 3 2 1
  3 5 7 9 0 9 7 5 3
   8 2 6 9 9 6 2 8
    0 8 5 8 5 8 0
     8 3 3 3 3 8
      1 6 6 6 1
       7 2 2 7
        9 4 9
         3 3
    GRAPH
    run_test(input, expected_output)
  end

  it "calculates and prints the correct numerology graph for '5 a 4 b 3 c 2 a 1 1a2b3c4d5e'" do
    input = '5 a 4 b 3 c 2 a 1 1a2b3c4d5e'
    expected_output = <<-GRAPH
 5 4 3 2 1 1 2 3 4 5
  9 7 5 3 2 3 5 7 9
   6 2 8 5 5 8 2 6
    8 0 3 0 3 0 8
     8 3 3 3 3 8
      1 6 6 6 1
       7 2 2 7
        9 4 9
         3 3
    GRAPH
    run_test(input, expected_output)
  end

end

RSpec.configure do |config|
  config.color = true
  config.formatter = :progress
end
