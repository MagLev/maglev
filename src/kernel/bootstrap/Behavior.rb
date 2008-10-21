class Behavior

  primitive_nobridge 'include', 'includeRubyModule:'
  primitive 'alias_method', 'rubyAlias:from:'

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
