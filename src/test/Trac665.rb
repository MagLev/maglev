# Trac665.rb  test of the RubyFlipNode  runtime support
#   these are called :flip2 and :flip3  S expressions  in  MRI parser

 arr = []
 10.times { |i| arr << i if (i == 4)..(i == 7) }
 unless arr == [4, 5, 6, 7] ; raise 'err'; end

  arr = []
  10.times { |i| arr << i if (i == 4)...(i == 4) }
  unless arr == [4, 5, 6, 7, 8, 9] ; raise 'err'; end

  arr = []
  10.times { |i| arr << i if (i == 4)...(i == 5) }
  unless arr == [4, 5] ; raise 'err'; end

  arr = [] 
  blk = Proc.new { |i| arr << i if (i == 4)...(i == 4) }
  8.times(&blk)
  8.times(&blk)
  unless arr == [4, 5, 6, 7, 0, 1, 2, 3, 4] ; raise 'err'; end

  class C
    def ma(arr)
      10.times { |i| arr << i if (i == 4)...(i == 4) }
    end
  end

  b = []
  o = C.new
  o.ma(b)
  o.ma(b)
  unless b == [4, 5, 6, 7, 8, 9, 4, 5, 6, 7, 8, 9] ; raise 'err'; end
  puts 'OK'
  true
