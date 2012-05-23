class RubyCipher
  # see also http://www.ruby-lang.org/ja/man/?cmd=view;name=OpenSSL%3A%3ACipher%3A%3ACipher
  def cipher(input, output, action, ciphertype, key)
    filter = OpenSSL::Cipher::Cipher.new(ciphertype)
    filter.encrypt if action == 'encrypt'
    filter.decrypt if action == 'decrypt'
    filter.pkcs5_keyivgen(key)
    before = File.open(input,"rb").read
    File.open(output,"wb") do |out|
      out.write filter.update(before)
      out.write filter.final
    end
  end
end
