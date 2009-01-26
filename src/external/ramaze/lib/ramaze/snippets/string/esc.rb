require 'cgi'
require 'uri'

module Ramaze
  module CoreExtensions

    # Extensions for String

    module String

      # String#escape is an extensible escaping mechanism for string.  currently
      # it suports
      #   '<div>foo bar</div>'.esc(:html)
      #   'foo bar'.esc(:uri)
      #   'foo bar'.esc(:cgi)

      def escape which = :html
        case which
        when :html
          Rack::Utils.escape_html(self)
        when :cgi
          Rack::Utils.escape(self)
        when :uri
          ::URI.escape(self)
        else
          raise ArgumentError, "do not know how to escape '#{ which }'"
        end
      end

      alias_method 'esc', 'escape'
    end

  end
end
