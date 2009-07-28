specs = [['String 10',   10], ['String 11',   11]]
specs.reject! do |x,|
  p x
  raise "Expecting Stirng but got #{x.inspect}" unless x.kind_of? String
end

