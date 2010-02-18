# From Sinatra 1.0.a tilt code
#
# MagLev gets a local jump error doing the instance_eval.

class C
  attr_accessor :foo
end

def evaluate(scope, &block)
  string = "self.foo = ( yield ).to_s"
  puts "evaluate: '#{string}' for #{scope.inspect}"
  scope.instance_eval string, "eval file", 10
end

def render(scope, &block)
  evaluate(scope, &block)
end

scope = C.new
render(scope) { 438 }
fx = scope.foo 
unless fx == '438' ; raise 'error'; end
true
