#  Run a block 10 times, where the block creates 
#   5MB of 1K strings and 10 times that much garbage
#   Needs at least 250MB gemstone GEM_TEMPOBJ_CACHE_SIZE

Bench.run [1] do |n|
  a = nil
  10.times do |i|
    a = Array.new(5000)
    a.length.times do |x|
      str = ""
      20.times{|j| str << "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYy"}
      a[x] = str
      10.times do |y|
        junk = ""
        20.times{|k| str << "GaRbAgEGaRbAgEGaRbAgEGaRbAgEGaRbAgEGaRbAgEGaRbAgE_"}
      end
    end
  end
end
