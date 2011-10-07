flags = File::CREAT | File::TRUNC | File::WRONLY
f = File.new("sssss", flags)

# Clean up file
File.delete("sssss")

#################### Trac Info
# ID:         507
# Summary:    File.new does not accept numeric modes
# Changetime: 2009-04-29 16:41:55+00:00
###

#  {{{
#  flags = File::CREAT | File::TRUNC | File::WRONLY
#  f = File.new("sssss", flags)
#  }}}
#  
#  The error:
#  
#  
#  {{{
#  $ mruby pbm.rb
#  ERROR 2094, The object 1537 was not of the expected class String.The object 1537 was not of the expected class String.
#  }}}
#  
#  