
case 'Accept: */*'
when /^([A-Za-z0-9!\#$%&'*+\-.^_`|~]+):\s*(.*?)\s*\z/om
  #puts "======= $1: '#{$1}'  $2: '#{$2}'"
  raise "FAILURE: $1 in when" unless $1 == 'Accept'
  raise "FAILURE: $2 in when" unless $2 == '*/*'
else
  raise "FAILURE: regexp failed in when"
end

# If the code below this comment is run before the previous code, then $1
# and $2 are set as a side-effect, and the above code "passes".  The code
# below does pass in MagLev, i.e., the regexp is working and setting $1 $2
# properly.  The problem appears to be a case of when to set $~.
'Accept: */*' =~ /^([A-Za-z0-9!\#$%&'*+\-.^_`|~]+):\s*(.*?)\s*\z/om
raise "FAILURE: $1" unless $1 == 'Accept'
raise "FAILURE: $2" unless $2 == '*/*'




#################### Trac Info
# ID:         283
# Summary:    $1, $2 not getting set for regexp in case statement
# Changetime: 2008-12-16 22:59:41+00:00
###

#  Distilled from Webrick:
#  
#  MRI passes this test, but MagLev fails:
#  
#  
#  {{{
#  
#  case 'Accept: */*'
#  when /^([A-Za-z0-9!\#$%&'*+\-.^_`|~]+):\s*(.*?)\s*\z/om
#    puts "======= $1: '#{$1}'  $2: '#{$2}'"
#    raise "FAILURE: $1 in when" unless $1 == 'Accept'
#    raise "FAILURE: $2 in when" unless $2 == '*/*'
#  else
#    raise "FAILURE: regexp failed in when"
#  end
#  
#  # If the code below this comment is run before the previous code, then $1
#  # and $2 are set as a side-effect, and the above code "passes".  The code
#  # below does pass in MagLev, i.e., the regexp is working and setting $1 $2
#  # properly.  The problem appears to be a case of when to set $~.
#  'Accept: */*' =~ /^([A-Za-z0-9!\#$%&'*+\-.^_`|~]+):\s*(.*?)\s*\z/om
#  raise "FAILURE: $1" unless $1 == 'Accept'
#  raise "FAILURE: $2" unless $2 == '*/*'
#  
#  }}}
#  
#  
#  