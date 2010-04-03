# Trac685.rb  , implement remove_instance_variable

class CA
  def self.setv(v)
    @foo = v
  end
  def setv(v)
    @foo = v
  end
  def self.remove
    remove_instance_variable( '@foo')
  end
  def remove
    remove_instance_variable( '@foo')
  end
end

c = CA.new
[ c, CA ].each { |o|
  if o.instance_variable_defined?( '@foo') ; raise 'error'; end
  o.setv(55)
  unless o.instance_variable_defined?( '@foo') ; raise 'error'; end
  unless (vx = o.remove) == 55 ; raise 'error'; end
  begin
    vy = o.remove
    raise 'error' # should not be here
  rescue NameError
    # ok
  end
}
true
