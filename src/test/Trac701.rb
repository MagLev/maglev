class Parent
  def offset
    10
  end
  private :offset
end

class Child < Parent
end
class Child
  public :offset
end

unless Child.new.offset == 10; raise 'fail'; end

true
