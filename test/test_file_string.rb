#!/usr/bin/env ruby
$:.unshift File.join(ENV['SCRIPTS'], 'lib') unless ENV['SCRIPTS'] == nil
$:.unshift File.join(File.dirname(__FILE__), '..', 'lib')

require 'rubygems'
require 'test/unit'
require 'tempfile'
require 'file_string'

class Test_FileString < Test::Unit::TestCase
  def setup
    @testfile = Tempfile::new("test.txt")
  end

  def test_append
    src      = 'hoge'
    expect   = "hoge\n"

    3.times do
      CommonUtil::FileString.append(@testfile, src)
    end

    open(@testfile) { |file|
      while line = file.gets
        assert_equal(expect, line)
      end
    }
  end

  def test_replace
    src      = 'hoge'
    try      = 'fuga'
    expect   = "fuga\n"

    3.times do
      CommonUtil::FileString.append(@testfile, src)
    end
    CommonUtil::FileString.replace(@testfile, src, try)

    open(@testfile) { |file|
      while line = file.gets
        assert_equal(expect, line)
      end
    }
  end

  def test_deleteString
    src      = 'hoge'
    replace  = 'fuga'
    erase    = 'hoge'
    expect   = "fuga\n"

    3.times do
      CommonUtil::FileString.append(@testfile, src)
    end
    CommonUtil::FileString.replace(@testfile, src, replace)
    2.times do
      CommonUtil::FileString.append(@testfile, src)
    end
    CommonUtil::FileString.delete(@testfile, erase)

    open(@testfile) { |file|
      while line = file.gets
        assert_equal(expect, line)
      end
    }
  end

  def teardown
    @testfile.close(true)
  end
end
