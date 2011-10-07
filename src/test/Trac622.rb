begin
  exit
rescue SystemExit => e
  unless 0 == e.status
    raise "Failed Test case: expecting 0 but got #{e.status}"
  end
end
true


#################### Trac Info
# ID:         622
# Summary:    Kernel.exit raises SystemError with status 1 instead of 0
# Changetime: 2009-11-03 21:28:32+00:00
###

#  the onliner script below exits with status 1
#  
#  {{{
#  # exit.rb
#  exit
#  }}}
#  
#  
#  {{{
#  $> maglev-ruby exit.rb
#  $> echo $?
#  1 # should be 0
#  }}}
#   
#  testcase attached
#  