# simplified coverage for Trac 753

class C
  def ma(&block)
    block = 
    if block.arity != 0
      y = Proc.new { $aa = "procOne" ; puts $aa }
    else
      y = Proc.new { $aa = "procZero"  ; puts $aa }
    end
    x = block
    @routes = x
  end
  def mb
    @routes.call
  end
end

$aa = 0
o = C.new
o.ma { $aa = "wrongProcZ" ; puts $aa } 
o.mb
unless $aa == 'procOne' ; raise 'fail' ; end
$aa = 0
o.ma { |o| $aa = "wrongProcOne"; puts $aa } 
o.mb
unless $aa == 'procOne' ; raise 'fail' ; end
true
