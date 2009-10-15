#  NOTE   NOTE     NOTE   NOTE     NOTE   NOTE     NOTE   NOTE
# This file is sensitive to line numbers.  If you add or delete any
# lines, then you'll have to up adjust the expected results accordingly.
#
# The line numbers reported by maglev are off in some places from those
# reported by MRI.  This test currently checks against the incorrect line
# numbers, since we want to detect changes to caller, not standards
# adherence.

require File.expand_path('simple', File.dirname(__FILE__))

# A method that will generate bridge methods with ':', '*' and '&' suffix
# characters.
def a(foo,*args,&block)
  b
end

def b
  c
end

def c
  x = nil
  [1].each do |i|
    x = caller
  end
  x
end

back_trace = a(1,2,3)

# END of line number sensitive code

expected =
  ["src/test/CallerTest.rb:23:in `c'",
   "src/test/CallerTest.rb:26:in `each'",
   "src/test/CallerTest.rb:26:in `c'",
   "src/test/CallerTest.rb:19:in `b'",
   "src/test/CallerTest.rb:15:in `a'",
   "src/test/CallerTest.rb:23"]

# Since the file names in the stack trace are different when invoked from
# command line and from vm-tests, we just match the common suffix
expected.each_with_index do |frame, i|
  ex_file, ex_line, ex_meth = frame.split(':')
  file, line, meth = back_trace[i].split(':')

  file_result = file.match /#{ex_file}/
  test(file_result.nil?, false, "file: expected[#{i}]: #{frame}  actual: #{file}")
  test(line, ex_line, "line: expected[#{i}]: #{frame}")
  test(meth, ex_meth, "meth: expected[#{i}]: #{frame}")
end
#test(back_trace, expected, "Backtrace")

report

# MRI produces:
# ["src/test/CallerTest.rb:24:in `each'",
#  "src/test/CallerTest.rb:24:in `c'",
#  "src/test/CallerTest.rb:19:in `b'",
#  "src/test/CallerTest.rb:15:in `a'",
#  "src/test/CallerTest.rb:39"]

# Maglev produces:
# ["src/test/CallerTest.rb:23:in `c'",      # Bogus frame should not be reported
#  "src/test/CallerTest.rb:25:in `each'",   # Line number should be 24
#  "src/test/CallerTest.rb:25:in `c'",      # Line number should be 24
#  "src/test/CallerTest.rb:19:in `b'",      # OK
#  "src/test/CallerTest.rb:15:in `a'",      # OK
#  "/Users/pmclain/GemStone/snapshots/MagLev-2009-09-01/src/test/CallerTest.rb:32"] # Line #

