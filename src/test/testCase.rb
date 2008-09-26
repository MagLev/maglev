def test3(n)
  ret = 0
  case n
    when :abc, :def
      ret = ret + 100
    when :zoo
      ret = ret + 1000
    else
      ret = ret + 10000
  end 
  ret
end
   
x = test3(:def)
raise 'ERR' unless x == 100
x = test3(:abc)
raise 'ERR' unless x == 100
x = test3(:zoo)
raise 'ERR' unless x == 1000
x = test3(:foo)
raise 'ERR' unless x == 10000

x = 55
b = [ 'a', 'c' ]
case 'z'
  when *['a']
    x = "foo"
  when *b
    x = "fooB"
  when *['z']
    x = "bar"
end
unless x == 'bar'
  raise 'ERR'
end

z = 66
case 'z'
  when 'a'
    z = "foo"
  when  'e' , *['z', 'w' ]
    z = "bar"
end
unless z == 'bar'
  raise 'ERR'
end

x = (case 
 when true, false
    'foo'
  end )
raise 'ERR' unless x == 'foo'

true
