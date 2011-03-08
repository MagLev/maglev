# Found doing Shellwords.shellwords("emacsclient -n")
#
# Maglev gets $1 messed up from previous match, and sets snippet to "em"
#
# Maglev prints:
#   $ maglev-ruby bug.rb
#   snippet: emacsclient
#   snippet: em
#   ERROR 2702 , FAIL (RubyRuntimeError)
#
# MRI prints:
#
#   $ ruby bug.rb
#   snippet: emacsclient
#   snippet: -n

def shellwords(line)

  until line.empty?

    loop do
      if line.sub!(/\A([^\s\\'"]+)/, '')
        flag = true
        snippet = $1
      else
        line.lstrip!
        break
      end
      puts "snippet: #{snippet}"
      raise "FAIL" if snippet == "em"
    end

  end

end

shellwords("emacsclient -n")
