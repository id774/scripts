#!/usr/bin/env ruby

########################################################################
# waitlock_test.rb: Test suite for waitlock.rb
#
#  Description:
#  This test suite verifies the behavior of the waitlock.rb script.
#  It tests proper argument handling and lockfile-waiting behavior.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with the -h option.
#    - Shows usage and exits when no arguments are provided.
#    - Prints an error and exits when an invalid number of arguments (only one) is given.
#    - Waits while a lockfile exists and proceeds correctly once the lockfile is removed.
#
#  Dependencies:
#  This test script requires the RSpec library. To install RSpec, run:
#      gem install rspec
#
#  Running the tests:
#  Execute the test script from the command line:
#      rspec test/waitlock_test.rb
#
#  Version History:
#  v1.0 2025-06-24
#      Initial release. Covers argument validation and lock-waiting logic.
#
########################################################################

require 'rspec'
require 'stringio'
require 'tempfile'
require 'open3'

describe 'waitlock.rb' do
  let(:script_path) { File.expand_path('../../waitlock.rb', __FILE__) }

  before(:each) do
    @original_argv = ARGV.dup
    @original_stdout = $stdout
    $stdout = StringIO.new
  end

  after(:each) do
    ARGV.replace(@original_argv)
    $stdout = @original_stdout
  end

  it 'shows usage when -h option is given' do
    output = `ruby #{script_path} -h`
    expect($?.exitstatus).to eq(0)
    expect(output).to include('Usage')
  end

  it 'shows usage with no arguments' do
    ARGV.clear
    load script_path
    expect { main }.to raise_error(SystemExit)
    output = $stdout.string
    expect(output).to include('waitlock.rb')
  end

  it 'prints error when only one argument is given' do
    ARGV.replace(['dummy.lock'])
    load script_path
    expect { main }.to raise_error(SystemExit)
    output = $stdout.string
    expect(output).to include('[ERROR] Two arguments required')
  end

  it 'waits for lockfile to be removed' do
    Dir.mktmpdir do |tmpdir|
      lockfile = File.join(tmpdir, "testlock_#{Time.now.to_i}")
      File.write(lockfile, "")  # create lockfile

      # Start the script in a subprocess and capture output
      output = ""
      Open3.popen2e("ruby", script_path, lockfile, "1") do |stdin, stdout_err, wait_thr|
        sleep 1  # Give it time to enter wait state
        File.delete(lockfile)  # Simulate unlock
        output = stdout_err.read
        wait_thr.value  # wait for process to finish
      end

      expect(output).to include("Waiting for lock file #{lockfile} to be released")
        .or include("Lock file released, proceeding...")
    end
  end
end

RSpec.configure do |config|
  config.color = true
  config.formatter = :progress
end
