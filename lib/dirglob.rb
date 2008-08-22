class DirGlob
  def DirGlob.dir(dir, sub_directory)
    array = Array.new
    if (FileTest.file?(dir))
      array << dir
    else
      if sub_directory
        dir = dir + "/**/"
      else
        dir = dir + "/"
      end
      Dir::glob(dir).each do |d|
        if (FileTest.directory?(d))
          Dir::foreach(d) do |f|
            if (FileTest.file?(d+f))
              array << d+f
            end
          end
        end
      end
    end
    return array
  end
end
