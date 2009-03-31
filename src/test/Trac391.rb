# From the hilbert bench mark.
#
# When we require mathn, then "5 / 2" becomes a rational number.
# MRI defines both to_i and to_int for the Rational, and so
# when you have this:
#
#    some_array[5 / 2]
#
# MRI calls to_int on it and gets 2, which is a proper index.  MagLev
# doesn't have to_int defined for rationals, and so it drops out of the
# primitive and tries to interpret the parameter as a Range object...

require 'mathn'

x = 5 / 2
raise "Bad class #{x.class}" unless x.class == Rational
raise "Bad to_i" unless x.to_i == 2
raise "Bad to_int" unless x.to_int == 2
