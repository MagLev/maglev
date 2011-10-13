layout = true
case layout
when FalseClass
  raise "Fail FalseClass"
when NilClass, TrueClass
  puts "OK: Nil or TRUE"
else
  raise "Fail ELSE"
end



layout = false
case layout
when FalseClass
  puts "OK: Nil or TRUE"
when NilClass, TrueClass
  raise "Fail NilClass, TrueClass"
else
  raise "Fail ELSE"
end


layout = nil
case layout
when FalseClass
  raise "Fail FalseClass"
when NilClass, TrueClass
  puts "OK: Nil or TRUE"
else
  raise "Fail ELSE"
end
#################### Trac Info
# ID:         514
# Summary:    case statement breaks with true
# Changetime: 2009-05-29 23:50:13+00:00
###

#  From Rails:
#  
#  {{{
#  layout = true
#  case layout
#  when FalseClass
#    raise "Fail FalseClass"
#  when NilClass, TrueClass
#    puts "OK: Nil or TRUE"
#  else
#    raise "Fail ELSE"
#  end
#  }}}
#  
#  Maglev goes down the else path, not the NilClass, TrueClass path