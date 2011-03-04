
class C
  def self.test
    $w = 0 ; $x = 0
    a = [ 'firstStr' , 'secondStr', 'thirdStr' , 'fourthStr' ]
    p = Proc.new { |o| puts "GCa of #{o}" ; $w += 100; }
    ObjectSpace.define_finalizer(a[0], p );
    ObjectSpace.define_finalizer(a[1], p );
    ObjectSpace.define_finalizer(a[2]) { |o| puts "GCb of #{o}" ; $x += 200; }
    ObjectSpace.define_finalizer(a[3]) { |o| puts "GCb of #{o}" ; $x += 200; } # should not execute
    ObjectSpace.define_finalizer(a[0]) { |o| puts "GCc of #{o}" ;  $w += 1000; }
    ObjectSpace.undefine_finalizer(a[1])
    a[0] = nil
    a[1] = nil
    a[2] = nil
    s = 'abc'
    (1.. 4000000).each { |n|  
      s << '00000000000000000000000000000dddddddddddddddddddddddddddddddddddddd' 
      if s.size > 100000
        s = 'abc'
      end
    }
    ww = $w
    xx = $x
    unless ww == 1100 ; raise 'error'; end
    unless xx == 200 ; raise 'error'; end
  end
end
C.test
true
