# See pickaxe p 359
#

# A return from within a lambda returns to the calling context, so this
# method should continue past the l.call line.
def test_from_lambda
  l = lambda { return 99 }
  res = l.call
  raise "lambda returned incorrect #{res}" unless res == 99
  return 88 # return something different from the lambda
end
raise "FAIL: lambda" unless test_from_lambda == 88

# proc goes like lambda
def test_from_deprecated_proc
  l = proc { return 99 }
  res = l.call
  raise "proc returned incorrect #{res}" unless res == 99
  return 88 # return something different from the lambda
end
raise "FAIL: deprecated proc" unless test_from_deprecated_proc == 88

# TODO: Currently, the next test case fails in MagLev with:
#
#     topaz 1> error during /Users/pmclain/MagLev/maglev-git/src/test/ReturnFromBlocksAndProcsTest.rb
#     -----------------------------------------------------
#     GemStone: Error         Nonfatal
#     Creating an instance of class ExecBlock is not allowed.
#     Error Category: [GemStone] Number: 2111 Arg Count: 1
#     Arg 1: ExecBlock

# A return from a proc object, Proc.new, returns from the calling context,
# so it should not get past the p.call line
def test_from_raw_proc
  p = Proc.new { return 99 }
  p.call
  raise "FAIL: should not get here..."
end
raise "FAIL: raw proc" unless test_from_raw_proc == 99

