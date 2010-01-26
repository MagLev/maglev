class C
  @@c_var = 12
end
c = C.new

$aa = 4 
begin
  class << c
    # MRI complains on this, but MagLev gets 12
    $aa = @@c_var
  end
rescue NameError
  $aa = 9
end
unless $aa == 9 ; raise 'error'; end
true
