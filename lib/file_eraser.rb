require 'dirglob'

class FileEraser
  def erase(path, hours)
    deltime = Time.now - (hours.to_i * 60 * 60)
    DirGlob.dir(path, true).each do |f|
      t = File.stat(f)
      if (t.mtime <= deltime)
        puts "#{t.mtime.strftime("%Y/%m/%d-%X")} #{t.size} #{f}"
        File.unlink(f)
      end
    end
    puts deltime.strftime("Older than %Y/%m/%d-%X in " + path + " has been deleted.")
  end
end
