if defined? Maglev
else
  $:.unshift
end
require 'psych'


Psych.dump("foo")

