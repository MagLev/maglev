module W
  def what(where=95)
    yield where
  end
end

class C1
  include W
end

class C2 < C1
  def what(where=98)
    super where 
  end
end

o = C2.new
$v454 = 0
o.what { | arg | $v454 = arg }
unless $v454 == 98 ; raise 'error'; end 
true
