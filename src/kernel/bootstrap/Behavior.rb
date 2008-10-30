class Behavior

  primitive_nobridge 'include', 'includeRubyModule:'
  primitive 'alias_method', 'rubyAlias:from:'

  def attr_accessor(*names)
    names.each do |n|
        attr_reader(n)
        attr_writer(n)
    end
  end

  def attr_reader(*names)
    names.each do |n|
        module_eval "def #{n}; @#{n}; end"
    end
  end

  def attr_writer(*names)
    names.each do |n|
        module_eval "def #{n}=(v); @#{n} = v; end"
    end
  end

  def module_eval(str)
    RUBY.module_eval(str, self)
  end

  def protected(name)
  end

  def private(name)
  end

  def public(name)
  end

  def alias(name)
  end

  def inspect
    name
  end

  def to_s
    name
  end

  def const_get(name)
    name
  end

end
