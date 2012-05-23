class WgetProxy
  require "net/http"
  require 'uri'

  def get(address, path, savefile)
    Net::HTTP.version_1_2

    if ENV['HTTP_PROXY'].nil?
      httpclass = Net::HTTP.new(address, 80)
    else
      proxy = URI.parse(ENV['HTTP_PROXY'])
      httpclass = Net::HTTP::Proxy(proxy.host, proxy.port).new(address, 80)
    end

    httpclass.start do |http|
      response = http.get(path)
      File.open(savefile,"wb") do |file|
        file.write response.body
      end
    end
  end
end
