# From rack-mount.  MagLev was reporting:
#    ERROR 2023, Error, 'too many arguments' (ArgumentError)
# doing    RegexpWithNamedGroups.new(/\A\/rails\/info\/properties\Z/)

class RegexpWithNamedGroups < Regexp
  def self.new(regexp) #:nodoc:
    super
  end

  # Wraps Regexp with named capture support.
  def initialize(regexp)
    regexp = Regexp.compile(regexp) unless regexp.is_a?(Regexp)
    source, options = regexp.source, regexp.options
    super(source, options)
  end
end

# Original bug was with a string argument
rx = RegexpWithNamedGroups.new('GET')
raise 'error A' unless rx.class == RegexpWithNamedGroups

# but we dealt with strings and regexp arguments differently, so this
# became a bug:
rx = RegexpWithNamedGroups.new(/\A\/rails\/info\/properties\Z/)
raise 'error B' unless rx.class. == RegexpWithNamedGroups

# And now we test the cases where a subclass has a two arg initialize or no
# initialize
class RegexpX < Regexp
  def self.new(regexp) #:nodoc:
    super
  end
end
rx = RegexpX.new(/\A\/rails\/info\/properties\Z/)
raise 'error C' unless rx.class. == RegexpX

class RegexpY < Regexp
  def self.new(regexp) #:nodoc:
    super(regexp, regexp.options)
  end

  # Wraps Regexp with named capture support.
  def initialize(regexp, opts)
    regexp = Regexp.compile(regexp) unless regexp.is_a?(Regexp)
    source, options = regexp.source, regexp.options
    super(source, options)
  end
end
rx = RegexpY.new(/\A\/rails\/info\/properties\Z/)
raise 'error C' unless rx.class == RegexpY

true
