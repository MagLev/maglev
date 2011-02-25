# Based on bug found doing: maglev-gem build foo.gemspec
# MagLev doesn't assign the gemspec variable correctly.
class Spec
  puts "Start def self.load"
  def self.load
    puts 'A'
    gemspec = 88
    #nil.pause
    ev_str = "Spec.new do ; puts 'in spec#initialize'; end"

    # before fix, assignment to gemspec does happen direct to the VC,
    #   but is overwritten by the write of eval's copy of
    #   gemspec back to the VC in the generated epilog of the VC,
    #    see below
    @@gather = proc { |gs| gemspec = gs ; puts 'B' }
    puts "Start Eval"
    eval ev_str
    xx = gemspec      # should not be nil
    puts 'C'
    # nil.pause
    xx
  end

  def initialize
    yield self if block_given?
    @@gather.call(98) if @@gather
  end
end

spec = Spec.load
raise "Fail" unless spec == 98
true

