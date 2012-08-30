# https://github.com/MagLev/maglev/issues/140
#
# defined? ::A::B doesn't work if A is not defined
#
# doing this on MagLev tries to resolve up to the last part of a
# Module definition and checks only if that is defined. It should
# instead check for each part.  This causes a problem when trying to
# use Tilt with Haml, as Tilt checks defined? ::Haml::Engine before
# loading Haml

defined?(::A::B)
# should not raise
