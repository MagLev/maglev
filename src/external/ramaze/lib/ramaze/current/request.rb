#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'rack/request'

module Ramaze

  # The purpose of this class is to act as a simple wrapper for Rack::Request
  # and provide some convinient methods for our own use.

  class Request < ::Rack::Request
    class << self

      # get the current request out of STATE[:request]
      #
      # You can call this from everywhere with Ramaze::Request.current

      def current() Current.request end
    end

    # you can access the original @request via this method_missing,
    # first it tries to match your method with any of the HTTP parameters
    # then, in case that fails, it will relay to @request

    def method_missing meth, *args
      key = meth.to_s.upcase
      return env[key] if env.has_key?(key)
      super
    end

    # the full request URI provided by Rack::Request e.g. http://localhost:7000/controller/action?foo=bar.xhtml

    def request_uri
      env['REQUEST_URI'] || path_info
    end

    # the IP address(s) making the request provided by Rack::Request. You shouldn't trust it

    def ip
      if addr = env['HTTP_X_FORWARDED_FOR']
        addr.split(',').last.strip
      else
        env['REMOTE_ADDR']
      end
    end

    # Request is from a local network?
    # Checks both IPv4 and IPv6

    ipv4 = %w[ 127.0.0.1/32 192.168.0.0/16 172.16.0.0/12 10.0.0.0/8 169.254.0.0/16 ]
    ipv6 = %w[ fc00::/7 fe80::/10 fec0::/10 ::1 ]
    LOCAL = (ipv4 + ipv6).map{|a| IPAddr.new(a)} unless defined?(LOCAL)

    # returns true if the IP address making the request is from local network.
    # Optional argument address can be used to check any IP address.

    def local_net?(address = ip)
      addr = IPAddr.new(address)
      LOCAL.find{|range| range.include?(addr) }
    rescue ArgumentError => ex
      raise ArgumentError, ex unless ex.message == 'invalid address'
    end

    def [](key, *rest)
      return params[key.to_s] if rest.empty?
      [key, *rest].map{|k| params[k.to_s] }
    end

    # Sets any arguments passed as @instance_variables for the current action.
    #
    # Usage:
    #   request.params # => {'name' => 'manveru', 'q' => 'google', 'lang' => 'de'}
    #   to_ivs(:name, :q)
    #   @q    # => 'google'
    #   @name # => 'manveru'
    #   @lang # => nil

    def to_ivs(*args)
      instance = Action.current.instance
      args.each do |arg|
        next unless value = self[arg]
        instance.instance_variable_set("@#{arg}", value)
      end
    end

    # Wrapping Request#params to support a one-level hash notation.
    # It doesn't support anything really fancy, so be conservative in its use.
    #
    # See if following provides something useful for us:
    # http://redhanded.hobix.com/2006/01/25.html
    #
    # Example Usage:
    #
    #  # Template:
    #
    #  <form action="/paste">
    #    <input type="text" name="paste[name]" />
    #    <input type="text" name="paste[syntax]" />
    #    <input type="submit" />
    #  </form>
    #
    #  # In your Controller:
    #
    #  def paste
    #    name, syntax = request['paste'].values_at('name', 'syntax')
    #    paste = Paste.create_with(:name => name, :syntax => syntax)
    #    redirect '/'
    #  end
    #
    #  # Or, easier:
    #
    #  def paste
    #    paste = Paste.create_with(request['paste'])
    #    redirect '/'
    #  end

    def params
      return {} if put?
      return @ramaze_params if @ramaze_params

      begin
        @rack_params ||= super
      rescue EOFError => ex
        @rack_params = {}
        Log.error(ex)
      end

      @ramaze_params = {}

      @rack_params.each do |key, value|
        if key =~ /^(.*?)(\[.*\])/
          prim, nested = $~.captures
          ref = @ramaze_params

          keys = nested.scan(/\[([^\]]+)\]/).flatten
          keys.unshift prim

          keys.each_with_index do |k, i|
            if i + 1 >= keys.size
              ref[k] = value
            else
              # in case the value is a string we cannot let it be ref next
              # time, so throw it away
              if ref[k].is_a?(String)
                ref = ref[k] = {}
              else
                ref = ref[k] ||= {}
              end
            end
          end
        else
          @ramaze_params[key] = value
        end
      end

      @ramaze_params
    end

    # Interesting HTTP variables from env

    def http_vars
      env.reject{ |k,v|
        k.to_s !~ /USER|HOST|REQUEST|REMOTE|FORWARD|REFER|PATH|QUERY|VERSION|KEEP|CACHE/
      }
    end

    # Returns a string presentation of the request, useful for debugging
    # parameters of the action.

    def to_s
      p, c, e = params.inspect, cookies.inspect, http_vars.inspect
      %{#<Ramaze::Request params=#{p} cookies=#{c} env=#{e}>}
    end
    alias inspect to_s

    # Pretty prints current action with parameters, cookies and
    # enviroment variables.

    def pretty_print pp
      p, c, e = params, cookies, http_vars
      pp.object_group(self){
        { 'params' => params,
          'cookies' => cookies,
          'env' => http_vars }.each do |name, hash|
          pp.breakable
          pp.text " @#{name}="
          pp.nest(name.length+3){ pp.pp_hash hash }
        end
      }
    end

    # Answers with a subset of request.params with only the key/value pairs for
    # which you pass the keys.
    # Valid keys are objects that respond to :to_s
    #
    # Example:
    #   request.params
    #   # => {'name' => 'jason', 'age' => '45', 'job' => 'lumberjack'}
    #   request.subset('name')
    #   # => {'name' => 'jason'}
    #   request.subset(:name, :job)
    #   # => {'name' => 'jason', 'job' => 'lumberjack'}

    def subset(*keys)
      keys = keys.map{|k| k.to_s }
      params.reject{|k,v| not keys.include?(k) }
    end

    # Is this an SSL request?
    def ssl?
      env['HTTPS'] == 'on' || env['HTTP_X_FORWARDED_PROTO'] == 'https'
    end

    # Returns 'https' if this is an SSL request and 'http' otherwise.
    def protocol
      ssl? ? 'https' : 'http'
    end

    def domain(path = '/')
      host = env['HTTP_HOST']
      URI("#{protocol}://#{host}#{path}")
    end

    # Returns and array of locales from env['HTTP_ACCEPT_LANGUAGE].
    # e.g. ["fi", "en", "ja", "fr", "de", "es", "it", "nl", "sv"]

    def locales
      env['HTTP_ACCEPT_LANGUAGE'].to_s.split(/(?:,|;q=[\d.,]+)/)
    end
  end
end
