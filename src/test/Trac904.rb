# Trac 904 

class EnumProblem
  def each
    enum_for(:each)
  end
end

e = EnumProblem.new.each
puts "#{e.inspect}"
unless e.class.equal?( Enumerable::ObjectEnumerator ) ; raise 'fail'; end
true

