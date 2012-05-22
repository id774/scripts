class FileHandling
  def self.appendString(file,str)
    if File.exist?(file)
      f = open(file,'a')
    else
      f = open(file,'w')
    end
    f << str
    f << "\n"
    f.close
  end

  def self.replaceString(f,src,out)
    f.rewind
    body = f.read
    body = body.gsub(src) { |tmp|
      out
    }
    f.rewind
    f.puts body
  end

  def self.replaceFile(file,src,out)
    open(file,"r+") { |f|
      f.flock(File::LOCK_EX)
      replaceString(f,src,out)
      f.truncate(f.tell)
    }
  end
end
