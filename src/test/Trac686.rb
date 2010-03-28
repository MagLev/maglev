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
end

pat = []
C.new.zero(:one, :long, "pre", true, *pat) # complete lint 1469
