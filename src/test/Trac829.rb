# Based on bug found doing: maglev-gem build foo.gemspec
# MagLev doesn't assign the gemspec variable correctly.
class Spec
  def self.load
    gemspec = nil
    ev_str = "Spec.new do ; puts 'in spec#initialize'; end"

    # The assignment to gemspec in the following proc does not take effect.
    @@gather = proc { |gs| gemspec = gs }
    eval ev_str
    gemspec      # should not be nil
  end

  def initialize
    yield self if block_given?
    @@gather.call(self) if @@gather
  end
end

spec = Spec.load
raise "Fail" if spec.nil?

