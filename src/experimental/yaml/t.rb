require 'psych'
p Psych.dump("foo")

p Psych.dump([:a, :b, :c])

p Psych.dump ({:a => "A", :b => "B"})
