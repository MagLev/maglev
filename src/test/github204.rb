class X
  def self.inherited klass
    klass.boom!
    super
  end

  def self.boom!
    @anything = nil
  end
end

raise "wrong name" unless Class.new(X).name =~ /#<Class:0x[a-f0-9]+>/
