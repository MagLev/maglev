# From Sinatra
class B
  def render_erb(template, data, options, locals, &block)
    "Renderx"
  end
  def foo
    'foox'
  end
end
b = B.new

r = b.__send__(:foo)   # works
unless r == 'foox' ; raise 'error' ; end
r = b.__send__(:render_erb, nil, nil, nil, nil)  
unless r == 'Renderx' ; raise 'error' ; end
true
