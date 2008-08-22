require 'md5'

class MD5Sum
  def md5sum(f)
    md5 = MD5.new
    while mb = f.read(1024 * 1024)
      md5.update(mb)
    end
    md5.hexdigest
  end
end
