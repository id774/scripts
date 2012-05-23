require 'dirglob'

class Touch
  def utime(filename, datetime, execute)
    puts "#{filename}"
    File::utime(datetime, datetime, "#{filename}") if execute
  end

  def touch(path, datetime, execute, sub_directory, touch_directory)
    puts "Set timestamp to #{datetime.strftime("%Y/%m/%d-%X")}"
    if touch_directory
      utime(File.expand_path(path), datetime, execute)
    else
      DirGlob.dir(path, sub_directory).each do |f|
        utime(f, datetime, execute)
      end
    end
    puts "Touched." if execute
    puts "But not touch, show only." unless execute
  end
end
