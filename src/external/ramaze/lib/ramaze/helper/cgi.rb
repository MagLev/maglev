#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  # Shortcuts to some CGI methods

  module Helper::CGI

    # shortcut for Rack::Utils.escape
    def url_encode(*args)
      Rack::Utils.escape(*args.map{|a| a.to_s })
    end

    # shortcut for Rack::Utils.unescape
    def url_decode(*args)
      Rack::Utils.unescape(*args.map{|a| a.to_s })
    end

    # shortcut for Rack::Utils.escape_html
    def html_escape(string)
      Rack::Utils.escape_html(string)
    end

    # shortcut for CGI.unescapeHTML
    def html_unescape(string)
      ::CGI.unescapeHTML(string.to_s)
    end

    # safely escape all HTML and code
    def h(string)
      Rack::Utils.escape_html(string).gsub(/#([{@$]@?)/, '&#35;\1')
    end

    # one-letter versions help in case like #{h foo.inspect}
    # ERb/ERuby/Rails compatible
    alias u url_encode
  end
end
