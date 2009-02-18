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
