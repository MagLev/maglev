class Module
  def attr_reader(*names)
    bnd = Binding.__basic_new(self)
    bnd.__set_lex_scope(LexicalPath.new)
    fake_vcglbl = [nil,nil]
    names.each do |n|
      the_name = self.__attr_type_check(n)
      str = "def #{the_name}; @#{the_name}; end"
      self.__module_eval_string( str, fake_vcglbl, bnd )
    end
  end

  def attr_writer(*names)
    bnd = Binding.__basic_new(self)
    bnd.__set_lex_scope(LexicalPath.new)
    fake_vcglbl = [nil,nil]
    names.each do |n|
      the_name = self.__attr_type_check(n)
      str = "def #{the_name}=(v); @#{the_name} = v; end"
      self.__module_eval_string( str, fake_vcglbl, bnd )
    end
  end
end
