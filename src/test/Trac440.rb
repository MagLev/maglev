

module Test
  autoload( "TestClass", File.expand_path('Trac440_test2.rb', File.dirname(__FILE__)))
  
  m = self
  d = defined?(TestClass)
  class SecondClass
    self.extend TestClass::Mod
  end
end

unless Test::SecondClass.one == 111 ; raise 'error'; end
true
