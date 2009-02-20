module Pbm
  def self.define_class(name)
    cl = Class.new(SystemCallError)
    const_set(name, cl)
    cl
    $a = cl
  end
end

kb = Pbm.define_class(:EBADF)

unless Pbm.constants.grep(/EBADF/)
  puts "Constant not defined Pbm::EBADF"
  raise 'error'
end

unless kb.superclass == SystemCallError
  puts "Bad superclass #{kb.superclass}"
  raise 'error'
end

ka = $a
kstderr = StandardError
nstd = kstderr.name
cn = kb.name
unless cn == 'Pbm::EBADF'
  puts "Bad class name #{kb.name}"
  raise 'error'
end
true
