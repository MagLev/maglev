# Reproduce a bug found in Haml
#
# MagLev was printing "ExecBlock", rather than the value of the ExecBlock

class C
  def x(result)
    result
  end

  def push_text(x)
    x
  end

  def adjust_tabs(n)
  end
end

_hamlout = C.new

# MagLev fails on result2: it contains "ExecBlock" rather than "fred barney"
result1 = eval "_hamlout.push_text(\"<p>\#{_hamlout.x((['fred', 'barney'].join(' ')\n));}</p>\\n\")", binding
result2 =  eval "_hamlout.push_text(\"<p>\#{_hamlout.adjust_tabs(1); _hamlout.x((['fred', 'barney'].join(' ')\n));}</p>\\n\")", binding

raise "Failed 1, got: #{result1.inspect}" unless result1 == "<p>fred barney</p>\n"
raise "Failed 2, got: #{result2.inspect}" unless result2 == "<p>fred barney</p>\n"
