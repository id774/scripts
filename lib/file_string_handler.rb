module FileStringHandler
  def self.appendString(file, str)
    if File.exist?(file)
      f = open(file, 'a')
    else
      f = open(file, 'w')
    end
    f << str
    f << "\n"
    f.close
  end

  def self.replaceFile(f, src, out)
    f.rewind
    body = f.read
    body = body.gsub(src) { |tmp|
      out
    }
    f.rewind
    f.puts body
  end

  def self.replaceString(file, src, out)
    open(file,"r+") { |f|
      f.flock(File::LOCK_EX)
      replaceFile(f,src,out)
      f.truncate(f.tell)
    }
  end

  def self.deleteString(file, str)
    out = ""
    IO.foreach(file) { |line|
      out << line unless line.include?(str)
    }
    open(file,"w") { |f|
      f.write out
    }
  end
end
