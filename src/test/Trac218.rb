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
#################### Trac Info
# ID:         218
# Summary:    unresolved Ruby global Errno::ENOENT
# Changetime: 2008-11-14 19:02:15+00:00
###

#  core/file/unlink_spec.rb (and others) produced the following error:
#  
#  User defined error, 'unresolved Ruby global Errno::ENOENT'
#  
#  Version: 20429-422