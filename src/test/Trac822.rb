# Another one from Tilt....
module Tilt
  module CompileSite
  end

  class Template
    def compile_template_method(method_name)
      CompileSite.class_eval <<-RUBY, 'xxx', 10
        def #{method_name}(locals)
          class << self
            this, locals = Thread.current[:tilt_vars]
            this.instance_eval do
              p locals
              raise "Fail: nil locals" if locals.nil?
            end
          end
        end
      RUBY
    end
  end
end

class App
  include Tilt::CompileSite    
end

t = Tilt::Template.new
t.compile_template_method('foo')  
a = App.new
h = {:user1 => nil}
Thread.current[:tilt_vars] = [a,h]
a.foo(27)
