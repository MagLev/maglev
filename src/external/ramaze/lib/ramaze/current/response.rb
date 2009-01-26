#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  class Response < Rack::Response
    class << self
      # Alias for Current::response
      def current() Current.response end
    end

    def initialize(body = [], status = 200, header = {}, &block)
      header['Content-Type'] ||= Global.content_type
      header['Accept-Charset'] = Global.accept_charset if Global.accept_charset
      super
    end

    # Build/replace this responses data
    def build(new_body = body, status = status, header = header)
      header.each do |key, value|
        self[key] = value
      end

      self.body, self.status = new_body, status
    end

    def body=(obj)
      if obj.respond_to?(:stat)
        @length = obj.stat.size
        @body = obj
      elsif obj.respond_to?(:size)
        @body = []
        @length = 0
        write(obj)
      else
        raise(ArgumentError, "Invalid body: %p" % obj)
      end
    end
  end
end
