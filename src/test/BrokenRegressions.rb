# This file holds test cases that are currently broken.  When they get
# fixed, they should be moved into FixedRegressions.rb, which is included
# in vmunit.conf, so that we can ensure we don't regress on these ad-hoc
# cases.

# ##### Trac # 239 #######################################################
# This one if from pp.rb (pretty print).  They try to mixin a bunch of
# stuff into object, but in maglev, object is committed.
#
# This regression fails like:
#
#    $ maglev-ruby src/test/BrokenRegressions.rb
#    topaz 1>
#    error during /Users/pmclain/maglev/git/src/test/BrokenRegressions.rb
#    -----------------------------------------------------
#    GemStone: Error         Nonfatal
#    User defined error, 'insert class disallowed, receiver is committed'
#    Error Category: [GemStone] Number: 2318 Arg Count: 2
#    Arg 1: halt
#    Arg 2: insert class disallowed, receiver is committed

module Mixin
  # nothing
end

class Object
  include Mixin
end
