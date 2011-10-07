

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
#################### Trac Info
# ID:         440
# Summary:    autoload not acting enough like require
# Changetime: 2009-04-22 15:46:03+00:00
###

#  The attached files show that replacing autoload with require works, while using autoload fails.  Until autoload is actually implemented it should do an immediate require of the referenced file.  Autoload is used by many packages, active_record in the case I encountered.