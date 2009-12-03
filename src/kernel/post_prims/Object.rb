# This part of Object in post_prims phase 
# because @tainted is a dynamic instVar
# and primitives load phase disallows creation of dynamic instVars.
#
class Object
  def taint
    # if frozen, primitive signals TypeError for gemstone error 2031
    @tainted = true
    self
  end

  def tainted?
    @tainted._equal?(true)
  end

  def untaint
    # if frozen, primitive signals TypeError for gemstone error 2031
    @tainted = false
    self
  end
end
