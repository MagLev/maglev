# Distilled from Thor::Group
#
# It looks like MagLev thinks that "options" in the 
class C
  attr_accessor :options
  class << self
    def invoke_from_option()
      options = 10
      verbose = 11
      name = "fred"
      class_eval <<-METHOD, __FILE__, __LINE__
        def fred
          o = options
          o
        end
        METHOD
    end
  end
end

C.invoke_from_option()
c = C.new
c.options = 25
result = c.fred
raise "Fail" unless result == 25
true
