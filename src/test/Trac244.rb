# ##### Trac # 244 #######################################################
# There are two problems in this file:
#
#   A: MagLev raises 'invalid arg' unless the exception parameter to raise
#     is a known exception.
#
#  B: Defining the error in another module, but same compilation unit seems
#     to go unnoticed by MagLev
#
module Gem244
  class LoadError < ::LoadError; end
end

module M244
  def foo
    # Bug A: MRI accepts an unknown exception type as a parameter to raise,
    # but MagLev complains
    raise FooError   # if false

    # Bug B: Even though Gem::LoadError is defined above, MagLev still has
    # problems here.
    raise Gem244::LoadError # if false
  end
end
