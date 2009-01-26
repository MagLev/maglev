require 'rubygems'
require 'socket'
require 'digest'
require 'json'
require 'cgi'

module Facebook
  class Error < StandardError
  end

  class APIProxy
    instance_methods.each { |m| undef_method m unless m =~ /^__/ }

    def initialize name, client
      @name, @client = name, client
    end

    def method_missing method, opts = {}
      @client.call "#{@name}.#{method}", opts
    end
  end

  class Client
    include Ramaze::Trinity if defined? Ramaze

    def initialize keepalive = true
      @keepalive = keepalive
      @proxies = {}
    end

    %w[ auth fbml feed fql friends notifications profile users pages events groups photos marketplace ].each do |n|
      define_method(n){ @proxies[n] ||= APIProxy.new(n, self) }
    end

    def call method, opts = {}
      args = { :api_key => KEY,
               :call_id => Time.now.to_f,
               :format => 'JSON',
               :v => '1.0',
               :session_key => params[:session_key] || SESSION,
               :method => method }.merge(opts).map{ |k,v|
                                                    "#{k}=" + case v
                                                              when Hash
                                                                v.to_json
                                                              when Array
                                                                v.join(',')
                                                              else
                                                                v.to_s
                                                              end
                                                  }.sort

      data = Array["sig=#{Digest::MD5.hexdigest(args.join+SECRET)}", *args].join('&')

      begin
        ret = post(data)
      rescue Errno::ECONNRESET, Errno::EPIPE
        @server = connect
        retry
      end while ret.empty? and @server = connect

      ret = case
            when ret == 'true':    true
            when ret == 'false':   false
            when ret[0..0] == '"': ret[1..-2]
            else
              begin
                JSON::parse(ret)
              rescue JSON::ParserError
                puts "Error parsing #{ret.inspect}"
                raise
              end
            end

      unless method == 'fql.query'
        ret = ret.first if ret.is_a? Array and ret.size == 1 and ret.first.is_a? Hash
      end

      raise Facebook::Error, ret['error_msg'] if ret.is_a? Hash and ret['error_code']

      ret
    ensure
      unless @keepalive
        @server.close
        @server = nil
      end
    end

    def valid?
      return false unless respond_to?(:request) and not request['fb_sig'].nil?
      request['facebook.valid?'] ||= \
        request['fb_sig'] == Digest::MD5.hexdigest(request.params.map{|k,v| "#{$1}=#{v}" if k =~ /^fb_sig_(.+)$/ }.compact.sort.join+SECRET)
    end

    def [] key
      params[key]
    end

    def redirect url
      url[0,0] = URL unless url =~ /^http/
      if respond_to?(:response)
        response.build "<fb:redirect url='#{url}'/>"
        throw :respond
      else
        "<fb:redirect url='#{url}'/>"
      end
    end

    def addurl goto = '/'
      "http://apps.facebook.com/add.php?api_key=#{KEY}&next=#{CGI.escape '?next='+goto}"
    end

    def params
      return {} unless valid?
      request['facebook'] ||= \
        request.params.inject({}) { |h,(k,v)|
          next h unless k =~ /^fb_sig_(.+)$/
          k = $1.to_sym

          case k.to_s
          when 'friends'
            h[k] = v.split(',').map{|e|e.to_i}
          when /time$/
            h[k] = Time.at(v.to_f)
          when 'expires'
            v = v.to_i
            h[k] = v>0 ? Time.at(v) : v
          when 'user'
            h[k] = v.to_i
          when /^(position_|in_|is_|added)/
            h[k] = v=='1'
          else
            h[k] = v
          end
          h
        }
    end

    private

    def connect
      @socket.close if @socket
      TCPSocket.new('api.facebook.com', 80)
    end

    def post data
      @server ||= connect

      @server.puts "POST /restserver.php HTTP/1.1\r\n"
      @server.puts "Host: api.facebook.com\r\n"
      @server.puts "Connection: keep-alive\r\n" if @keepalive
      @server.puts "Content-Type: application/x-www-form-urlencoded\r\n"
      @server.puts "Content-Length: #{data.length}\r\n"
      @server.puts "\r\n#{data}\r\n"
      @server.puts "\r\n\r\n"

      buf = ''
      while @server.gets
        if $_ == "\r\n"
          @server.gets
          if $_.strip! == '0'
            @server.gets
            break
          end
          buf << @server.read($_.to_i(16))
        end
      end

      buf
    end
  end
end