# trac 312 , singleton on a Hash disturbs Smalltalk
#   perception of named size  of the instance of Hash .

h = { }
h[5] = 'abc'

ks = h.keys

unless ks == [5] ; raise 'err' ; end

def h.foobar
end

ks = h.keys

unless ks == [5] ; raise 'err' ; end
true
