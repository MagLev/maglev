# tests for RubyOpElementAsgnNode 
class C
  def initialize
    @iva = { }
    @ivb = [6]
    @getcount = 0
  end
  def geta
    @getcount = @getcount + 1
    @iva
  end
  def testa
    @ivc = 5
    cc = 6
    @ivc ||= 7
    unless @ivc == 5 ; raise 'err' ; end
    dd = nil
    dd &&= 5
    unless dd == nil ; raise 'err'; end
    dd ||= 7
    unless dd == 7 ; raise 'err'; end
    dd &&= 5
    unless dd == 5 ; raise 'err'; end
  end
  def testb
    @ivb[0] ||= 12
    unless @ivb[0] == 6 ; raise 'err' ; end
  end
  def testc
    unless @iva['a'] == nil ; raise 'err' ; end
    geta['a'] ||= 9 
    unless @iva['a'] == 9 ; raise 'err' ; end
    unless @getcount == 1 ; raise 'err' ; end
    geta['a'] ||= 10
    unless @iva['a'] == 9 ; raise 'err' ; end
    unless @getcount == 2 ; raise 'err' ; end
    geta['a'] &&= 11
    unless @iva['a'] == 11 ; raise 'err' ; end
    unless @getcount == 3 ; raise 'err' ; end
    geta['b'] &&= 11
    unless @iva['b'] == nil ; raise 'err' ; end
    unless @getcount == 4 ; raise 'err' ; end
  end
end 

o = C.new
o.testa
o.testb
o.testc
true
#################### Trac Info
# ID:         86
# Summary:    ||= and &&=  if LHS is an array reference, could be optimized
# Changetime: 2009-02-18 22:11:58+00:00
###

#  MRI accepts:
#  
#  {{{
#  cairo:maglev-git pmclain$ irb
#  >> r = []
#  => []
#  >> r[0] ||= 1
#  => 1
#  >> r[1] &&= 2
#  => nil
#  >> r[0] &&= 2
#  => 2
#  }}}
#  
#  MagLev does not accept either:
#  
#  {{{
#  topaz 1> maglev
#  >> r = []
#  => []
#  *> r[0] &&= 5
#  -----------------------------------------------------
#  GemStone: Error         Nonfatal
#  No method was found for the selector #'&&:' when sent to nil with
#  arguments contained in anArray( 5).
#  Error Category: [GemStone] Number: 2010 Arg Count: 3
#  Arg 1: nil
#  Arg 2: &&:
#  Arg 3: anArray
#  topaz 1> 
#  
#  
#  
#  topaz 1> maglev
#  >> r = []
#  => []
#  *> r[0] ||= 4
#  -----------------------------------------------------
#  GemStone: Error         Nonfatal
#  No method was found for the selector #'||:' when sent to nil with
#  arguments contained in anArray( 4).
#  Error Category: [GemStone] Number: 2010 Arg Count: 3
#  Arg 1: nil
#  Arg 2: ||:
#  Arg 3: anArray
#  topaz 1> 
#  
#  }}}
#  
#  