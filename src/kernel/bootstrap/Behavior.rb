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

  # map name to _rubyName so name will work for metaclasses
  primitive 'name' , '_rubyName'

  def private(*names)
    # TODO set visibility to private for specified methods
    #  if names empty, set default visibility for subsequent methods to private
  end

  def public(*names)
    # TODO set visibility to public for specified methods
    #  if names empty, set default visibility for subsequent methods to public
  end
  def protected(*names)
    # TODO set visibility to protected for specified methods
  end

  def alias(name)
    # TODO ?
  end

  def inspect
    name
  end

  def to_s
    name
  end

#   def const_get(name)
#     name
#   end

  primitive_nobridge '_allClassVars', 'allClassVarNames'

  def class_variable_defined?(aName)
    sym = aName.to_sym
    a = _allClassVars
    lim = a.length
    n = 0
    while (n < lim)
      if (sym.equal?(a[n]))
        return true
      end
      n = n + 1
    end
    false
  end

  def class_variables
    a = _allClassVars
    lim = a.length
    r = Array.new(lim)
    n = 0
    while (n < lim)
      r[n] = a[n].to_s
      n = n + 1
    end
    r
  end

end
