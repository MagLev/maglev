
class C
  def ma(arg=nil)
  end  

  def mb(arg) 
  end
end

begin
  C.new.ma(*[3,4])  # expect ArgumentError , too many args
rescue ArgumentError
  $ax = 11
end
begin
  C.new.mb(*[3,4])  # expect ArgumentError , too many args
rescue ArgumentError
  $bx = 22
end
unless $bx == 22 ; raise 'fail'; end
unless $ax == 11 ; raise 'fail'; end
true
