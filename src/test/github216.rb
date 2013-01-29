# https://github.com/MagLev/maglev/issues/216
#
# Ruby splat operator works on real array, not on copy.

def meth(*args)
  args.shift
end

array = [1, 2]

meth(*array)
raise "Array should still contain two values." if array.size != 2
