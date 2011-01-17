# MagLev is printing Smalltalk style error messages rather than ruby
# messages, and there is no stack trace printed.
#
# $ maglev-ruby pbm.rb
# ERROR 2702 , exception class/object expected: :fail (ArgumentTypeError)
#
# $ ruby pbm.rb
# /Users/pmclain/GemStone/dev/pbm.rb:11:in `raise': exception class/object expected (TypeError)
#   from /Users/pmclain/GemStone/dev/pbm.rb:11

raise :fail
