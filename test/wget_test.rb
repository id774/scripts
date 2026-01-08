#!/usr/bin/env ruby

########################################################################
# wget_test.rb: Test suite for wget.rb
#
#  Description:
#  This test suite verifies the behavior of the wget.rb script.
#  It ensures proper argument handling and mocks download logic.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Shows usage and exits with code 0 when invoked with the -h option.
#    - Exits with usage message when no arguments are given.
#    - Simulates downloading a file by mocking URI.open without network access.
#    - Writes downloaded content to a local file using mocked File.open.
#    - Ensures correct filename derivation from the given URL.
#
#  Dependencies:
#  This test script requires the RSpec library. To install RSpec, run:
#      gem install rspec
#
#  Running the tests:
#  Execute the test script from the command line:
#      rspec test/wget_test.rb
#
#  Version History:
#  v1.0 2025-06-24
#      Initial release. Covers usage error and mocked download test.
#
########################################################################

require 'rspec'
require 'stringio'

describe 'wget.rb' do
  let(:script_path) { File.expand_path('../../wget.rb', __FILE__) }

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

  it 'exits with usage when no arguments are given' do
    ARGV.clear
    load script_path
    expect { main }.to raise_error(SystemExit)
    output = $stdout.string
    expect(output).to include('wget.rb')
    expect(output).to include('Usage:')
  end

  it 'downloads and writes file content correctly (mocked)' do
    url = 'http://example.com/mockfile.txt'
    dummy_content = 'dummy file content'
    ARGV.replace([url])

    # Mocking URI.open to yield a StringIO with dummy content
    mock_source = double('source')
    allow(mock_source).to receive(:read).and_return(dummy_content)
    if URI.respond_to?(:open)
      allow(URI).to receive(:open).with(url).and_yield(mock_source)
    else
      allow(Kernel).to receive(:open).with(url).and_yield(mock_source)
    end

    # Mocking File.open to yield a file-like object
    mock_output = double('output')
    expect(mock_output).to receive(:print).with(dummy_content)
    allow(File).to receive(:open).with('mockfile.txt', 'w+b').and_yield(mock_output)

    load script_path
    main
  end
end

RSpec.configure do |config|
  config.color = true
  config.formatter = :progress
end
