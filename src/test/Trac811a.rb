# test fixes to ExecBlock>>_debugSourceForBlock and it's uses in 
#   define_method

class C
  def self.dm
    define_method('foo') { puts 10 }
  end
end

C.dm
cx = C
o = C.new
o.foo
