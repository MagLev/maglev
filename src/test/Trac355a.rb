module Pbm
  def self.define_class(name)
    klass = Class.new(SystemCallError)
    const_set(name, klass)
    klass
  end
end

klass = Pbm.define_class(:EBADF)

unless Pbm.constants.grep(/EBADF/)
  puts "Constant not defined Pbm::EBADF"
end

unless klass.superclass == SystemCallError
  puts "Bad superclass #{klass.superclass}"
end

unless klass.name == 'Pbm::EBADF'
  puts "Bad class name #{klass.name}"
end
