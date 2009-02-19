$const_name = :Baz
module ErrnoFoo
  class_eval %Q{
    class #{$const_name}
    end
  }
end
klass = ErrnoFoo.const_get($const_name)
raise "Expecting 'ErrnoFoo::Baz' but got #{klass.name}" unless klass.name == 'ErrnoFoo::Baz'
true
