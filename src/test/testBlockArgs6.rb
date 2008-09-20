class Yield
    def splat(*args)
      44.pause		# AA
      r = yield *args
    end

    #def two_args
    #  yield 1, 2
    #end

    #def two_arg_array
    #  yield [1, 2]
    #end
end


ret = nil
iy = Yield.new
iy.splat(1,2,3) {|*args| 
  ret = args 
  y = ret
  y.pause
}
55.pause
unless ret == [1,2,3]
  raise 'ERR'
end

 true
