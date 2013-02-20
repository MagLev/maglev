class String  
  def __interpolate_with_array(anArray)
    sprintf(self, *anArray)
  end

  def __interpolate_with_hash(aHash)
    # 
    interpolation_pattern = Regexp.union(
      /%\{(\w+)\}/,                               # matches placeholders like "%{foo}"
      /%<(\w+)>(.*?\d*\.?\d*[bBdiouxXeEfgGcps])/  # matches placeholders like "%<foo>.d"
    )

    interpolation_pattern_with_escape = Regexp.union(
      /%%/,
      interpolation_pattern
    )

    # % uses self (i.e. the String) as a format specification and returns the
    # result of applying it to the given arguments. In other words it interpolates
    # the given arguments to the string according to the formats the string
    # defines.
    #
    # There are three ways to use it:
    #
    # * Using a single argument or Array of arguments.
    #
    #   This is the default behaviour of the String class. See Kernel#sprintf for
    #   more details about the format string.
    #
    #   Example:
    #
    #     "%d %s" % [1, "message"]
    #     # => "1 message"
    #
    # * Using a Hash as an argument and unformatted, named placeholders.
    #
    #   When you pass a Hash as an argument and specify placeholders with %{foo}
    #   it will interpret the hash values as named arguments.
    #
    #   Example:
    #
    #     "%{firstname}, %{lastname}" % {:firstname => "Masao", :lastname => "Mutoh"}
    #     # => "Masao Mutoh"
    #
    # * Using a Hash as an argument and formatted, named placeholders.
    #
    #   When you pass a Hash as an argument and specify placeholders with %<foo>d
    #   it will interpret the hash values as named arguments and format the value
    #   according to the formatting instruction appended to the closing >.
    #
    #   Example:
    #
    #     "%<integer>d, %<float>.1f" % { :integer => 10, :float => 43.4 }
    #     # => "10, 43.3"
    pos = self =~ interpolation_pattern
    if pos.nil?
      raise ArgumentError.new('one hash required')
    end

    self.dup.gsub(interpolation_pattern_with_escape) do |match|
      if match == '%%'
        '%'
      else
        key = ($1 || $2).to_sym
        raise KeyError unless aHash.has_key?(key)
        $3 ? sprintf("%#{$3}", aHash[key]) : aHash[key]
      end
    end
  end

  def %(arg)
    if arg._isHash
      __interpolate_with_hash(arg)
    elsif arg._isArray
      __interpolate_with_array(arg)
    else
      arg = [ arg ]
      __interpolate_with_array(arg)
    end
  end
end
  