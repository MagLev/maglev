require File.expand_path('simple', File.dirname(__FILE__))
#     BEGIN TEST CASES

# The following lines just reference the global variables on page 334 of
# Pickaxe. This part of the test passes if the parser can handle them
# all...

# TODO: I've added x = in front of many of these variables, otherwise,
# maglev complains with:
#
#   topaz 1> error during StandardGlobalsTest.rb
#   GemStone Smalltalk Compiler Errors:
#   Error were found while recompiling instance method for class 'no category'
#    "method source not available"
#   error 1037, statement has no effect,
#
#  Ruby handles the non "x = " case fine.

# Test initial values
test($VERBOSE, false, "$VERBOSE initial value")
test($DEBUG,   false, "$DEBUG   initial value")

# Test $LOAD_PATH is an alias for $:
test($:, $LOAD_PATH, "initial are equal")
$: << 'foo'
test($:.include?('foo'), true, '$: << foo')
test($:, $LOAD_PATH, "after modify $: are equal")
$LOAD_PATH << 'bar'
test($:, $LOAD_PATH, "after modify $LOAD_PATH are equal")
test($:.include?('bar'), true, '$LOAD_PATH << foo')

# BEGIN GLOBAL PARSE CHECK
x = $!
x = $@
x = $:

x = $&
x = $+
x = $`
x = $'
x = $=
x = $1
x = $2
x = $3
x = $9
x = $~

x = $/
x = $-0
x = $\
x = $,
x = $.
x = $;
x = $<
x = $>
x = $_

x = $defout
x = $deferr
x = $-F
x = $stderr
x = $stdin
x = $stdout

x = $0
x = $*
x = $"
x = $$
x = $?
x = $:
x = $-a
x = $-d
x = $DEBUG
x = __FILE__
x = $F
x = $FILENAME
x = $-i
x = $-I
x = $-K
x = $-l
x = __LINE__
x = $LOAD_PATH
x = $-p
x = $SAFE
x = $VERBOSE
x = $-v
x = $-w

# x = ARGF
x = ARGV
x = ENV
x = false
x = nil
x = self
x = true

# x = DATA
# x = FALSE
# x = NIL
x = RUBY_PLATFORM
x = RUBY_RELEASE_DATE
x = RUBY_VERSION
x = STDERR
x = STDIN
x = STDOUT
# SCRIPT_LINES__ is commented out, since we don't support it yet
# x = SCRIPT_LINES__
# x = TOPLEVEL_BINDING
# x = TRUE
#  END GLOBAL PARSE CHECK

report
Gemstone.abortTransaction # Don't mess up $: etc.

# This occurrence of __END__ is necessary to define the global
# variable DATA above.
__END__
