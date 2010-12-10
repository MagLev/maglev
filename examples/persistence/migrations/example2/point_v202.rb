# A Two-Dimensional point, with Polar Coordinates (<r,theta>).
# This version removes the 1.0.0 compatibility methods.
Maglev.persistent do
  class Point
    undef_method :x
    undef_method :y
  end
end
Maglev.commit_transaction


