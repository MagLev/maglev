#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module Helper::Formatting

    # Format a floating number nicely for display.
    #
    # Usage:
    #   number_format(123.123)           # => '123.123'
    #   number_format(123456.12345)      # => '123,456.12345'
    #   number_format(123456.12345, '.') # => '123.456,12345'

    def number_format(n, delimiter = ',')
      delim_l, delim_r = delimiter == ',' ? %w[, .] : %w[. ,]
      h, r = n.to_s.split('.')
      [h.reverse.scan(/\d{1,3}/).join(delim_l).reverse, r].compact.join(delim_r)
    end

    # Answer with the ordinal version of a number.
    #
    # Usage:
    #   ordinal(1)   # => "1st"
    #   ordinal(2)   # => "2nd"
    #   ordinal(3)   # => "3rd"
    #   ordinal(13)  # => "13th"
    #   ordinal(33)  # => "33rd"
    #   ordinal(100) # => "100th"
    #   ordinal(133) # => "133rd"

    def ordinal(number)
      number = number.to_i

      case number % 100
      when 11..13; "#{number}th"
      else
        case number % 10
        when 1; "#{number}st"
        when 2; "#{number}nd"
        when 3; "#{number}rd"
        else    "#{number}th"
        end
      end
    end

    # stolen and adapted from rails
    def time_diff from_time, to_time = Time.now, include_seconds = false
      distance_in_minutes = (((to_time - from_time).abs)/60).round
      distance_in_seconds = ((to_time - from_time).abs).round if include_seconds

      case distance_in_minutes
        when 0..1
          return (distance_in_minutes == 0) ? 'less than a minute' : '1 minute' unless include_seconds
          case distance_in_seconds
            when 0..4   then 'less than 5 seconds'
            when 5..9   then 'less than 10 seconds'
            when 10..19 then 'less than 20 seconds'
            when 20..39 then 'half a minute'
            when 40..59 then 'less than a minute'
            else             '1 minute'
          end

        when 2..44           then "#{distance_in_minutes} minutes"
        when 45..89          then 'about 1 hour'
        when 90..1439        then "about #{(distance_in_minutes.to_f / 60.0).round} hours"
        when 1440..2879      then '1 day'
        when 2880..43199     then "#{(distance_in_minutes / 1440).round} days"
        when 43200..86399    then 'about 1 month'
        when 86400..525959   then "#{(distance_in_minutes / 43200).round} months"
        when 525960..1051919 then 'about 1 year'
        else                      "over #{(distance_in_minutes / 525960).round} years"
      end
    end

    # copied from actionpack
    AUTO_LINK_RE = %r{
                    (                          # leading text
                      <\w+.*?>|                # leading HTML tag, or
                      [^=!:'"/]|               # leading punctuation, or
                      ^                        # beginning of line
                    )
                    (
                      (?:https?://)|           # protocol spec, or
                      (?:www\.)                # www.*
                    )
                    (
                      [-\w]+                   # subdomain or domain
                      (?:\.[-\w]+)*            # remaining subdomains or domain
                      (?::\d+)?                # port
                      (?:/(?:(?:[~\w\+@%-]|(?:[,.;:][^\s$]))+)?)* # path
                      (?:\?[\w\+@%&=.;-]+)?     # query string
                      (?:\#[\w\-]*)?           # trailing anchor
                    )
                    ([[:punct:]]|\s|<|$)       # trailing text
                   }x unless defined? AUTO_LINK_RE

    # Turns all urls into clickable links.  If a block is given, each url
    # is yielded and the result is used as the link text.
    def auto_link(text, opts = {})
      html_options = ' ' + opts.map{|k,v| "#{k}='#{v}'"}.join(' ') if opts.any?
      text.gsub(AUTO_LINK_RE) do
        all, a, b, c, d = $&, $1, $2, $3, $4
        if a =~ /<a\s/i # don't replace URL's that are already linked
          all
        else
          text = b + c
          text = yield(text) if block_given?
          %(#{a}<a href="#{b=="www."?"http://www.":b}#{c}"#{html_options}>#{text}</a>#{d})
        end
      end
    end
    alias autolink auto_link

    # takes a string and optional argument for outputting compliance HTML
    # instead of XHTML.
    # e.g nl2br "a\nb\n\c" #=> 'a<br />b<br />c'

    def nl2br(string, xhtml = true)
      br = xhtml ? '<br />' : '<br>'
      string.gsub(/\n/, br)
    end

    # Maybe port to ruby < 1.8.7 ?
    def obfuscate_email(string)
      string = string.to_s
      text = string.each_byte.map{|c| "&#%03d" % c}.join
      %(<a href="mailto:#{string}">#{text}</a>)
    end

    # Returns Hash with tags as keys and their weight as value.
    #
    # Example:
    #     tags = %w[ruby ruby code ramaze]
    #     tagcloud(tags)
    #     # => {"code"=>0.75, "ramaze"=>0.75, "ruby"=>1.0}
    #
    # The weight can be influenced by adjusting the +min+ and +max+ parameters,
    # please make sure that +max+ is larger than +min+ to get meaningful output.
    #
    # This is not thought as immediate output to your template but rather to
    # help either implementing your own algorithm or using the result as input
    # for your tagcloud.
    #
    # Example:
    #     - tagcloud(tags).each do |tag, weight|
    #       - style = "font-size: %0.2fem" % weight
    #       %a{:style => style, :href => Rs(tag)}= h(tag)

    def tagcloud(tags, min = 0.5, max = 1.5)
      result = {}
      total = tags.size.to_f
      diff = max - min

      tags.uniq.each do |tag|
        count = tags.respond_to?(:count) ? tags.count(tag) : tags.select{|t| t==tag }.size
        result[tag] = ((count / total) * diff) + min
      end

      result
    end
  end
end
