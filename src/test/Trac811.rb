class Base
  class << self
    def compile!(verb, path, block)
      method_name = "#{verb} #{path}"
      define_method(method_name, &block)
      unbound_method = instance_method method_name
      remove_method method_name
      block.arity != 0 ?
        proc { unbound_method.bind(self).call(*@block_params) } :
        proc { unbound_method.bind(self).call }
    end

    def get(path, opts={ }, &block)
      @meth = compile!('GET', '/', block)
    end

    def invoke
      # instance_eval(@meth[0])
      m = @meth[0]
      puts "invoke: #{m}"
      b = self.new
      b.instance_eval(m)
    end

    def meth
      @meth
    end
  end

  def route
    m = Base.meth
    route_eval(&m)
  end

  def route_eval(&block)
    instance_eval(&block)
  end
end

Base.get '/' do
  puts "XXX"
end

b = Base.new
b.route
true
