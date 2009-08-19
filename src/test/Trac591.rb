# From Sinatra
class B
  def render_erb(template, data, options, locals, &block)
    puts "Render erb"
  end
  def foo
    puts :foo
  end
end
b = B.new

b.__send__(:foo)   # works
b.__send__(:render_erb, nil, nil, nil, nil)  # fails
