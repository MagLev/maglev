class BlankSlate
  class << self
    def hide(name)
      undef_method(name.to_s)
    end
  end
end

raise "fail before hide" if BlankSlate.instance_methods.grep(/^class$/).size != 1
cl = BlankSlate
BlankSlate.hide("class")
raise "fail after hide" if BlankSlate.instance_methods.grep(/^class$/).size != 0

# more coverage of mixed undef_method and remove_method
class CA
  def ma(a)
    a + 10 
  end
  def mb
  end
  def self.hide(sel)
    undef_method(sel)
  end
  def self.rmm(sel)
    remove_method(sel)
  end
end
class CB < CA
  def ma(a)
    a + 100
  end
  def mb
  end
end

o = CB.new
x = o.ma(5)
unless x == 105 ; raise 'error'; end
CB.rmm('ma')
x = o.ma(5)
unless x == 15 ; raise 'error'; end

CB.hide('ma')
ex = 0
begin
  o.ma(6)
rescue NoMethodError
  ex += 2
end
unless ex == 2 ; raise 'error'; end

begin
  CB.hide('ma')  # expect error , already hidden
rescue NameError
  ex += 2
end
unless ex == 4 ; raise 'error'; end

CB.hide('mb')
begin
  CB.rmm('mb')  # expect error, already undef-ed
rescue NameError
  ex += 2
end
unless ex == 6 ; raise 'error'; end

begin
  CB.hide('mc')  # no such method
rescue NameError
  ex += 2
end
unless ex == 8 ; raise 'error'; end

begin
  CB.rmm('mc')  # no such method
rescue NameError
  ex += 2
end
unless ex == 10 ; raise 'error'; end

class CB
  def ma(a)  # undo the hide
    a + 1000
  end
end
x = o.ma(4) 
unless x == 1004 ; raise 'error'; end
x = o.ma(6) { puts 'foo' }
unless x == 1006 ; raise 'error'; end
puts "Ok"
true


