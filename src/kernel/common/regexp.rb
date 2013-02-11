class Regexp
  def inspect
    # avoid use of gsub here so inspect can be used when debugging gsub
    src = source
    src_siz = source.size
    n = 0 
    str = '/'
    while n < src_siz
      ch = src[n]
      if ch.eql?( ?/ )
        if n > 0 && src[n-1].eql?( ?\\ )
          str << ch
        else
          str << "\\/"
        end
      else
        str << ch
      end
      n += 1
    end
    str << '/' 
    str << option_to_string(options)
    k = kcode()
    str << k[0,1] if k and k != "none"
    return str
  end
  def option_to_string(option)
    string = ""
    string << 'm' if (option & MULTILINE) > 0
    string << 'i' if (option & IGNORECASE) > 0
    string << 'x' if (option & EXTENDED) > 0
    string
  end
end
