class C
  def loop
    [:a, :b, :c].each do |sym|
      do_something(sym)
    end
  end

  def do_something(s)
    next   # Local Jump Error in MRI; OK in MagLev
  end
end

x = 0
begin
  C.new.loop
  x = 1
rescue LocalJumpError
  # expected
  x = 2
end
unless x == 2 ; raise 'failed'; end
true
