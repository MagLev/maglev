# Distilled from rails 3
#
# Aliasing a method from a super class, and then using super, yields
# different behavior in MagLev and MRI.  In our case, the Errors class
# aliases its superclass version of :[]= to :set. The implementation of
# Errors#[]= calls super, and apparently, MRI will call into HashProxy#[]=,
# but MagLev calls OrderedHash#[]=
#
# Running this script with MRI, we see the following trace:
#
#   $ ruby $pbm
#   -- Errors#[]=
#   -- OrderedHash#[]=
#   -- HashProxy#[]=
#   -- Errors#[]=
#
# Running this script with MagLev, we see the following trace:
#
#   $ mruby $pbm
#   -- Errors#[]=
#   -- OrderedHash#[]=      # <= This calls super...
#   -- OrderedHash#[]=      # <= ...which ends up here, rather than HashProxy#[]=
#   -- HashProxy#[]=
#   -- Errors#[]=
#
# To reproduce, run in maglev with -d, and the second time you hit the
# nil.pause, the problem is calling from stack frame 6 to 5.
#
#   topaz 1> wh
#   1 Exception >> signal                      (envId 0) @2 line 50   [methId 4332033]
#   2 Exception >> signalNotTrappable          (envId 0) @3 line 7   [methId 4332545]
#   3 Exception class >> signalNotTrappable    (envId 0) @3 line 3   [methId 4356609]
#   4 Object >> pause                          (envId 0) @2 line 7   [methId 2280961]
#   5 OrderedHash >> []=::&                    (envId 1) @5 line 3   [methId 40721665]
#   ==> 6 Errors >> []=::&                         (envId 1) @9 line 5   [methId 40720129]
#   7 Errors >> []=::                          (envId 1) @2 line 1   [methId 40718849]
#   8 Errors >> []:                            (envId 1) @7 line 5   [methId 40717569]
#   9 Errors >> []=::                          (envId 1) @4 line 3   [methId 40716289]
#   10 Object >> __compileFile                  (envId 1) @18 line 80   [methId 40715009]
#



# HashProxy class here just to "prove" MRI calls directly up from
# OrderedHash to HashProxy
class HashProxy < Hash
  def []=(k, v)
    puts "-- HashProxy#[]="
    super
  end
end

class OrderedHash < HashProxy
  def initialize(*args, &block)
    super
    @keys = []
  end

  def []=(key, value)
    puts "-- OrderedHash#[]="

    # The second time through this pause, the call from frame 6 to frame 5
    # is the problem
    # nil.pause if defined? Maglev

    @keys << key if !has_key?(key)
    super
  end

end

class Errors < OrderedHash
  def initialize(base)
    @base = base
    super()
  end

  alias_method :get, :[]
  alias_method :set, :[]=

  def [](attribute)
    if errors = get(attribute.to_sym)
      errors
    else
      set(attribute.to_sym, [])
    end
  end

  def []=(attribute, error)
    puts "-- Errors#[]="
    self[attribute.to_sym] << error
  end
end

h = Errors.new(Object)
h[:last_name] = []
raise "Wrong number of keys" unless h.keys.size == 1

h[:last_name] = []
raise "Wrong number of keys" unless h.keys.size == 1

