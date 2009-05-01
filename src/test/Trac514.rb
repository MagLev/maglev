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
