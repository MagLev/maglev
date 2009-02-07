++++ OBSOLETE FILE , no longer used

# NOTE: This file must be included AFTER Class.rb so that we can call new
# on Object.

# This file is here to define globaly useful stuff.  At the time this file
# was created, the bootstrapping was not far enough along to allow us to
# define these items in the ../kernel.rb file.  Placing these definitions
# in their own file seems to solve that bootstrapping problem.

# Sentinal value used to distinguish between nil as a value passed by the
# user and the user not passing anything for a defaulted value.  E.g.,:
#
#   def foo(required_param, optional_param=Undefined)
#     if optional_param.equal?( Undefined )
#       puts "User did not pass a value"
#     else
#       puts "Users passed #{optional_param} (which may be nil)"
#     fi
#   end
#
Undefined = Object.new

# Used to coerce a value to a string via to_str(), if available.
def StringValue(obj)
  Type.coerce_to(obj, String, :to_str)
end
#private :StringValue
