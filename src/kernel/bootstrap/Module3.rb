class Module
  def attr_reader(*names)
    bnd = Binding.__basic_new(self)
    bnd.__set_lex_scope(LexicalPath.new)
    fake_vcglbl = [nil,nil]
    args = [ bnd , '(attr_reader)', 1 ]
    names.each do |n|
      the_name = self.__attr_type_check(n)
      str = "def #{the_name}; @#{the_name}; end"
      self.__module_eval_string( str, fake_vcglbl, *args )
    end
  end

  def attr_writer(*names)
    bnd = Binding.__basic_new(self)
    bnd.__set_lex_scope(LexicalPath.new)
    fake_vcglbl = [nil,nil]
    args = [ bnd , '(attr_writer)', 1 ]
    names.each do |n|
      the_name = self.__attr_type_check(n)
      str = "def #{the_name}=(v); @#{the_name} = v; end"
      self.__module_eval_string( str, fake_vcglbl, *args )
    end
  end

  def include(a_module)
    # redefinition of bootstrap version from Module.rb
    a_module.append_features(self)
    if a_module.respond_to? :included
      a_module.included(self)
    end
    self
  end

  # Invoked as a callback when a_module includes receiver.
  def included(*args)
  end
  private :included

  # Callback invoked whenever the receiver is used to extend an object.
  # The object is passed as a paramter.
  def extended(*args)
  end
  private :extended
end
