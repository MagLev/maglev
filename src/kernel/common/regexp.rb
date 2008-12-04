class Regexp
  def inspect
    str = '/' << source.gsub("/", "\\/") << '/' << option_to_string(options)
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
