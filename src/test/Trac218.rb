require File.expand_path('simple', File.dirname(__FILE__))

test(File.delete(),       0, 'File.delete() => 0')
test(File.send(:delete),  0, 'File.send(:delete) => 0')
# The next one is the failure case for Trac 218:
test(File.send("delete"), 0, 'File.send("delete") => 0')


test(File.unlink(),       0, 'File.unlink() => 0')
test(File.send(:unlink),  0, 'File.send(:unlink) => 0')
# The next one is the failure case for Trac 218:
test(File.send("unlink"), 0, 'File.send("unlink") => 0')

report
