require File.expand_path('simple', File.dirname(__FILE__))

[nil, true, false].each do |o|
  test(o.tainted?, false, "#{o.inspect}.tainted? Initial")
  test(o.taint,        o, "#{o.inspect}.taint ")
  test(o.tainted?, false, "#{o.inspect}.tainted? After taint")  # shouldn't change
  test(o.untaint,      o, "#{o.inspect}.untaint ")
  test(o.tainted?, false, "#{o.inspect}.tainted? After untaint")
end

report

true
