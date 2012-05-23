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

  def test_appendString
    src      = 'hoge'
    expect   = "hoge\n"

    3.times do
      CommonUtil::FileString.appendString(@testfile, src)
    end

    open(@testfile) { |file|
      while line = file.gets
        assert_equal(expect, line)
      end
    }
  end

  def test_replaceString
    src      = 'hoge'
    try      = 'fuga'
    expect   = "fuga\n"

    3.times do
      CommonUtil::FileString.appendString(@testfile, src)
    end
    CommonUtil::FileString.replaceString(@testfile, src, try)

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
      CommonUtil::FileString.appendString(@testfile, src)
    end
    CommonUtil::FileString.replaceString(@testfile, src, replace)
    2.times do
      CommonUtil::FileString.appendString(@testfile, src)
    end
    CommonUtil::FileString.deleteString(@testfile, erase)

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
