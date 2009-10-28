class BlankSlate
  class << self
    def hide(name)
      undef_method(name.to_s)
    end
  end
end

raise "fail before hide" if BlankSlate.instance_methods.grep(/^class$/).size != 1
BlankSlate.hide("class")
raise "fail after hide" if BlankSlate.instance_methods.grep(/^class$/).size != 0
