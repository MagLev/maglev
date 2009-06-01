require File.expand_path('simple', File.dirname(__FILE__))

[nil, true, false, 1, 2.0].each do |o|
  test(o.tainted?, false, "#{o.inspect}.tainted? Initial")
  test(o.taint,        o, "#{o.inspect}.taint ")
  test(o.tainted?, false, "#{o.inspect}.tainted? After taint")  # shouldn't change
  test(o.untaint,      o, "#{o.inspect}.untaint ")
  test(o.tainted?, false, "#{o.inspect}.tainted? After untaint")
end
[1, 2.0].each do |x|
  test(x.tainted?, false, "#{x.inspect}.tainted? Numeric A")
  test(x.taint,        x, "#{x.inspect}.taint ")
  test(x.tainted?, false, "#{x.inspect}.tainted? Numeric B")
  test(x.untaint,      x, "#{x.inspect}.taint ")
end
report

true
