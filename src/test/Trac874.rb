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

  arr = []
  until line.empty?
    loop do
      # puts "before sub! line: #{line}"
      if line.sub!(/\A([^\s\\'"]+)/, '')
        flag = true
	ax = $0
        snippet = $1
        # puts "line: #{line} snippet: #{snippet}"
      else
        # puts "before lstrip! #{line} d1: #{$1.inspect}"
        line.lstrip!
        # puts "after lstrip! #{line}"
        break
      end
      puts "snippet: #{snippet}"
      arr << snippet
    end
  end
  arr
end

x = shellwords("emacsclient -n")
raise 'fail' unless x == ["emacsclient","-n"]
true
