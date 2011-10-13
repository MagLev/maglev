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
#################### Trac Info
# ID:         521
# Summary:    Regexp::ONCE  - test case needed
# Changetime: 2011-03-14 21:42:00+00:00
###

#   The  :dregex_once  Sexpression is not distinguished from :dregex .
#  This means that dynamic substitutions are being done each time a regex
#  with the 'o'  option is evaluated instead of just the first time.
#  
#  I am coding a fix now, but could use some help writing a test case.