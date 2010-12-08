class Name1
  ORIGINAL_NAME = :Name1
end


# @param old_parent [Module,Class]
def rename_class(old_fqn, new_fqn)

  old_parent, old_name = get_parent_and_name(old_fqn)
  new_parent, new_name = get_parent_and_name(old_fqn)

  return if old_parent == new_parent && old_name == new_name

  raise "Bad old_parent #{old_parent}" unless old_parent.kind_of?(Module)
  raise "Bad new_parent #{new_parent}" unless new_parent.kind_of?(Module)

  new_name = new_name.to_sym
  raise "#{new_name} already defined in #{new_parent}" if new_parent.const_defined?(new_name)

  old_name = old_name.to_sym
  klass = old_parent.send(:remove_const, old_name)
  raise "Couldn't find #{old_name} in #{old_parent}" unless klass

  new_parent.const_set(new_name, klass)
end

def rename_class(old_parent, old_name, new_parent, new_name)

  raise "Bad old_parent #{old_parent}" unless old_parent.kind_of?(Module)
  raise "Bad new_parent #{new_parent}" unless new_parent.kind_of?(Module)

  new_name = new_name.to_sym
  raise "#{new_name} already defined in #{new_parent}" if new_parent.const_defined?(new_name)

  old_name = old_name.to_sym
  klass = old_parent.send(:remove_const, old_name)
  raise "Couldn't find #{old_name} in #{old_parent}" unless klass

  new_parent.const_set(new_name, klass)
end

def get_parent_and_name(fqn)
  parent = nil
  child  = Object
  fqn.to_s.split('::').each do |n|
    parent = child
    child = child.const_get(n) unless n.empty?
  end
  [parent, child]
end

module M
end

rename_class(Object, 'Name1', M, :DifferentName)

raise "Couldn't find in new place" unless defined? M::DifferentName
raise "Not right class" unless M::DifferentName::ORIGINAL_NAME == :Name1
raise "Old name not gone" if defined? ::Name1
p M::DifferentName
