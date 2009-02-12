# Distilled from the interaction between rubygems and the standard
# rbconfig.rb file.


module C
  CONFIG = { 'BASERUBY' => 'ruby' }
end
RbC = C
$rbc_id = ::RbC.__id__

module G
  raise "at 1" unless ::RbC.__id__ == $rbc_id

  RbC = C unless defined?(::RbC)

  raise "at 2" unless ::RbC.__id__ == $rbc_id
  raise "at 3" unless RbC.__id__ == $rbc_id
  raise "at 4" unless RbC.equal?(::RbC)
end
