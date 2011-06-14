
class ThrTest
  $A = nil
  def self.Failed
    raise 'Error'
  end
  def self.test
    unless Thread.critical.equal?(false) ; Failed() ; end
    tmain = Thread.current
    s = tmain.alive? 
    unless s.equal?(true) ; 		Failed() ; end
    s = tmain.inspect
    puts s
    unless s[-6,6] = ', run>' ; 	Failed() ; end
    mainb = Thread.main
    unless tmain.equal?(mainb);       Failed() ; end
    t = Thread.fork { $A = 99 }
    x = $A
    unless x == 99 ; 			Failed() ; end
    s = t.alive? 
    unless s.equal?(false) ; 		Failed() ; end

    t = Thread.fork { $A = 98 ; Thread.pass ; $A = 88 }
    unless $A == 98 ; 			Failed() ; end 
    lis = Thread.list
    unless lis.size == 2 ;		Failed() ; end
    puts lis.inspect
    Thread.pass
    unless $A == 88 ;                   Failed() ; end 
    lis = Thread.list
    unless lis.size == 1 ;		Failed() ; end
    unless lis[0].equal?(tmain);	Failed() ; end

    tb = Thread.new(3,4,5) { |a,b,c| $A = [a,b,c]} 
    unless (x = $A) == [3,4,5];		Failed() ; end

    tb = Thread.new(3,4,5) { |*a| $A = a} 
    unless (x = $A) == [3,4,5];		Failed() ; end

    t = Thread.fork { $A = 78 ; Thread.stop ; $A = 68 }
    unless (x = $A) == 78 ; 			Failed() ; end
    Thread.pass
    unless (x = $A) == 78 ;                     Failed() ; end
    x = t.status
    unless x == 'sleep' ;                 Failed() ; end
    unless (x = t.stop?).equal?(true)  ; 		Failed() ; end
    unless t.alive?.equal?(true) ; 		Failed() ; end
   
    t.run
    unless (x = $A) == 68 ;                     Failed() ; end
    unless t.status.equal?(false) ; 		Failed() ; end
    unless t.alive?.equal?(false) ; 		Failed() ; end

    t = Thread.fork { $A = 48 ; Thread.stop ; $A = 58 }
    unless (x = $A) == 48 ;                     Failed() ; end
    t.exit
    Thread.pass
    unless (x = $A) == 48 ;                     Failed() ; end
    unless t.alive?.equal?(false) ;             Failed() ; end

    t = Thread.fork { $A = 98 ; Thread.stop ; $A = 58 }
    unless tmain.group.class.equal?(ThreadGroup);  Failed() ; end
    unless t.group.equal?(tmain.group);		  Failed() ; end

    s = t.join(1)
    unless (s == nil);				Failed() ; end
    unless (x = $A) == 98 ; 			Failed() ; end
    unless (s = t.status) == 'sleep';		Failed() ; end
    t.terminate
    Thread.pass 
    unless (x = $A) == 98 ; 			Failed() ; end
    unless t.alive?.equal?(false) ;             Failed() ; end

    t = Thread.fork { $A = 97 ; Thread.pass ; $A = 87 }
    unless (x = $A) == 97 ;                     Failed() ; end
    s = t.join(1)
    unless s.equal?(t) ;                     Failed() ; end
    unless (x = $A) == 87 ;                     Failed() ; end

    t = Thread.fork { $A = 96 ; Thread.pass ; $A = Thread.current.keys }
    unless t.keys == [] ;  			Failed() ; end
    t['B'] = 7
    t['C'] = 6
    s = t.join(1)
    act = IdentitySet.with_all($A) 
    exp = IdentitySet.with_all( [:B, :C] )
    unless act == exp  ;   		      Failed() ; end
    unless t.key?(:C).equal?(true) ;		      Failed() ; end
    unless t.key?(:D).equal?(false) ;		      Failed() ; end

    t = Thread.fork { $A = 20 ; Thread.pass; $A = $A + 20000 }
    unless $A == 20 ;							Failed() ; end
    unless (tp = t.priority) == 0; Failed() ; end # have seen failure with tp==25 here
					#   when ordering of vmunit.conf changed
    t.priority=(-5)
    tb = Thread.fork { $A = $A + 100 ; Thread.pass; $A = $A + 1000 }
    unless $A == 120;							Failed() ; end 
    Thread.pass
    unless $A == 1120;                                                   Failed() ; end
    Thread.pass
    unless $A == 1120;                                                   Failed() ; end
    t.priority=(0)
    Thread.pass 
    unless $A == 21120;                                                   Failed() ; end

    t = Thread.fork { $A = 48 ; Thread.stop ; $A = 58 }
    unless $A == 48 ;						Failed() ; end
    t.wakeup
    unless $A == 48 ;                                           Failed() ; end
    Thread.pass
    unless $A == 58 ;                                           Failed() ; end 

    t = Thread.fork { $A = 48 ; Thread.stop ; $A = 58 ; 456}
    t.wakeup
    unless (x = t.value) == 456 ;			        Failed() ; end

    #---------------
    t = Thread.fork { 
      puts "entered $A + 1000 "
      $A = 68 
      while $A == 68
        puts "sleep in $A + 1000 "
        sleep			# coverage for sleep(0) equivalent to pass 
        puts "woke in $A + 1000 "
      end
      $A = $A + 1000
      puts "Finished $A + 1000 "
    }
    unless $A == 68 ; 			Failed() ; end
    $A = $A + 100 
    puts "Main1  $A + 1000 "
    cnt = 0
    while ($A < 1000)
      if cnt == 0 ; puts "Main2  $A + 1000 " ; end
      cnt += 1
      Thread.pass
    end
    unless $A == 1168;			Failed() ; end
    #---------------
  end
end


ThrTest.test
true
