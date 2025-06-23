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
#  Version History:
#  v1.0 2025-06-24
#      Initial release. Covers argument validation and lock-waiting logic.
#
#  Dependencies:
#  This test script requires the RSpec library. To install RSpec, run:
#  `gem install rspec`
#
#  Running the tests:
#  Execute the test script from the command line:
#  `rspec test/waitlock_test.rb`
#
#  Test Cases:
#  - Exit with usage on no arguments
#  - Error on invalid argument count
#  - Correct waiting behavior for temporary lockfile
#
########################################################################

require 'rspec'
require 'stringio'
require 'tempfile'

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
    lockfile = File.join(Dir.tmpdir, "testlock_#{Time.now.to_i}")
    File.write(lockfile, "locked")
    ARGV.replace([lockfile, '1'])

    thread = Thread.new do
      sleep 0.01
      File.delete(lockfile)
    end

    allow(Kernel).to receive(:sleep) { |t| sleep 0.01 }

    load script_path
    main
    output = $stdout.string
    expect(output).to include("Waiting for lock file #{lockfile} to be released...")
    expect(output).to include("Lock file released, proceeding...")
    thread.join
  end
end

RSpec.configure do |config|
  config.color = true
  config.formatter = :progress
end
