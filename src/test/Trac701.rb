class Parent
  def offset
    10
  end
  private :offset
end

class Child < Parent
  public :offset
end

p Child.new.offset
