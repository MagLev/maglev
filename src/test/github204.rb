class X
  def self.inherited klass
    klass.boom!
    super
  end

  def self.boom!
    @anything = nil
  end
end

if RUBY_VERSION == "1.8.7"
  raise "wrong name" unless Class.new(X).name == ""
elsif RUBY_VERSION == "1.9.3"
  raise "wrong name" unless Class.new(X).name == nil
end
raise "wrong name" unless Class.new(X).inspect =~ /#<Class:0x[a-f0-9]+>/
