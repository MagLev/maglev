# test passes if it does not return an error.
[*100..1000].product([*100..1000]).map{|x,y| x*y}.select{|s|s=s.to_s; s==s.reverse}.max
