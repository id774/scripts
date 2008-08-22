class TwitterPost
  require 'net/http'
  require 'uri'

  def post(user, pass, status_utf8)
    address = 'twitter.com'
    Net::HTTP.version_1_2

    if ENV['HTTP_PROXY'].nil?
      httpclass = Net::HTTP.new(address, 80)
    else
      proxy = URI.parse(ENV['HTTP_PROXY'])
      httpclass = Net::HTTP::Proxy(proxy.host, proxy.port).new(address, 80)
    end

    req = Net::HTTP::Post.new('/statuses/update.json')
    req.basic_auth user,pass
    req.body = 'status=' + URI.encode(status_utf8)

    httpclass.start do |http|
      http.request(req)
    end
  end
end
