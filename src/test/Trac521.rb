# test for Regexp ONCE
Proc.new do |value|
  num = value + 1
  2.times do
    ret = /#{value += 1}/o =~ "#{num}"
    raise "ERROR" if ret.nil?
  end
  raise "ERROR" if num != value
end.call(10)
true
