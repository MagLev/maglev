#  Run a block 10 times, where the block creates 
#    40MB(32bit VM) or 80MB(64bit VM) of 100 element Arrays
#    and 10 times that much garbage.
#    Needs 150MB gemstone GEM_TEMPOBJ_CACHE_SIZE"

Bench.run [1] do |n|
  a = nil
  10.times do |i|
    a = Array.new(100_000)
    a.length.times do |x|
      a[x] = Array.new(100)
      10.times do |y|
        Array.new(100)
      end
    end
  end
end
