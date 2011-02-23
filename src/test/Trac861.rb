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

C.new.loop
