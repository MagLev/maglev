# From Rails 3 ActiveModel
module M
  protected

  def foo
    10
  end
end

class C
  include M

Maglev::System.session_temp_put(:TrapM, true)
  public :foo  # This doesn't work in MagLev
end

c = C.new
x = c.foo  # MagLev raises: NoMethodError: protected method `foo' for C,
unless x == 10 ; raise 'failed'; end
true
