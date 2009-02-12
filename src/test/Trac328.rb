#  use of foo(*v) seen in mspec frameork

def meth328(*x)
    unless x.class.equal?(Array) ; raise 'error'; end
    unless x[0] = 3280 ; raise 'error'; end
    3281
end

[3280].each do | v |
  x = meth328(*v)
  unless x == 3281 ; raise 'error'; end
end
puts "done"
true

