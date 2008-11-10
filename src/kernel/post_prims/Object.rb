# This part of Object in post_prims phase 
# because @tainted is a dynamic instVar
# and primitives load phase disallows creation of dynamic instVars.
#
class Object
  def taint
    raise TypeError, "Can't modify frozen object" if self.frozen?
    @tainted = true
  end

  def tainted?
    @tainted.equal?(true)
  end

  def untaint
    raise TypeError, "Can't modify frozen object" if self.frozen?
    @tainted = false
  end
end
