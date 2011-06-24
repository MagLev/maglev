# File Trac463a.rb  , 
# coverage of break as used in Sinatra , 
#  break out of a block with  ensure and rescue both active on the stack

class C
  def test
    assert_raise(EOFError) {  geta() }  
    ax = $ax
    bx = $bx
    unless [ ax , bx ] == [ 55, 66 ] ; raise 'fail'; end
  end

  def assert_raise(expected, &blk)
    actual_exception = nil
    got_ex = nil
    rx = assert_block() {
      begin
        yield
      rescue Exception => actual_exception
        got_ex = 95
        break 
      end
    }
    unless got_ex == 95 ; raise 'fail';end 
    unless actual_exception.is_a?( expected) ; raise 'fail'; end
  end

  def assert_block(&blk)
    rx = yield
  end

  def geta
    begin
      getb()
    ensure
      $ax = 55
    end
  end

  def getb
     begin
       getc()
     ensure
       $bx = 66
     end
  end

  def getc
    raise EOFError
  end

end

C.new.test
true
