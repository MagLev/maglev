$:.unshift File.dirname(__FILE__)

module M
  sx = self
  autoload :AttributeMethods, 'Trac765a.rb'

  module AttributeMethods   # MRI triggers autoload here; MagLev doesn't
  end
end

raise "FAIL" unless M::AttributeMethods::X == 123
true
