# From Rails 3 ActiveModel
module M
  protected

  def foo
    10
  end
end

class C
  include M

  public :foo  # This doesn't work in MagLev
end

c = C.new
c.foo  # MagLev raises: NoMethodError: protected method `foo' for C,
