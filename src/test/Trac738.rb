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

RegexpWithNamedGroups.new('GET')
