# Distilled from optparse.rb
#
# Maglev is not handling passing *[] correctly.
#
class C
  def zero(id, *args, &block)
    # MRI prints:     [:long, "pre", true]
    # MagLev prints:  [:long, "pre", true, []]
    p args     # MRI:
    raise "Wrong num args #{args.size} expecting 3" unless args.size == 3
  end
 
  def self.test
    pat = []
    self.new.zero(:one, :long, "pre", true, *pat) 
    true
  end
end

C.test
