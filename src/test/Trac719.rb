# From Rails 3
#
# MagLev was reporting instance methods from modules mixed into Object.
module Loadable
  def require_association
    10
  end
end

class Object
  include Loadable
end

m = Module.new do
  raise 'fail' if instance_methods.include?('require_association')
end

p m.instance_methods
raise 'fail' if m.instance_methods.include?('require_association')
