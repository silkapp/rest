require 'net/http'
require 'uri'
require 'rubygems'
require 'cgi'
require 'json'
require 'rexml/document'

print "Fix url encoding when this is going to be used"

def internalSilkRequest (api, method, url, params = {}, contentType = 'text/plain', dataType = :data, data = nil, headers = {})
  uri = URI(url + '?' + params.collect { |k,v| "#{k}=#{CGI::escape(v.to_s)}" }.join('&'))
  
  request = case method
    when :get    then Net::HTTP::Get.new(uri.request_uri)
    when :post   then Net::HTTP::Post.new(uri.request_uri)
    when :put    then Net::HTTP::Put.new(uri.request_uri)
    when :delete then Net::HTTP::Delete.new(uri.request_uri)
  end
  
  request.initialize_http_header(headers)
  request.body = data
  
  request['Content-Type'] = contentType
  request['Cookie'] = api.cookies.to_a.map { |kv| kv.join('=') } .join(';')

  case dataType
    when :data then request['Accept'] = 'text/plain'
    when :json then request['Accept'] = 'application/json'
    when :xml  then request['Accept'] = 'application/xml'
  end
    
  response = Net::HTTP.new(uri.host, uri.port).request request

  pdat = response.body

  if response.code == '200'
    pdat = case dataType
      when :json then JSON.parse response.body
      when :xml  then REXML::Document.new response.body
    end
  end
  
  return pdat, response
end

def mkJson (x)
  if x.class == Hash
    return JSON.generate x
  else
    return x.to_s
  end
end

module SilkApi
  class Api < BaseApi
    attr_accessor :cookies

    def initialize (apiURL)
      super(apiURL)
      @cookies = {}
    end
    
    def login (email, password)
      pdat, res = self.User.signin({'email' => email, 'password' => password})
      cookie = res['Set-Cookie']
      @cookies['silk_sid'] = cookie.sub /.*silk_sid=([^;]+).*/, '\1'
      @cookies['silk_tid'] = cookie.sub /.*silk_tid=([^;]+).*/, '\1'
      return pdat, res
    end
  end
end