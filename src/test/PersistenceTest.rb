# This does the hat trick

working_dir = File.dirname(__FILE__)

# A function to parse the output of a topaz run and remove all the extra
# garbage.  It is hardcoded to handle only a couple of output types, so the
# tests below are limited to nil, integers and strings.
def extract_return_value(topaz_garbage)
  # Typical output from topaz is:
  #
  #    topaz 1> 42
  #    true
  #    topaz 1>
  #
  # We want to capture the "42", so match against the first line
  topaz_garbage.split('\n')[0] =~ /topaz 1>\s*(.*)/
  str_value = $1
  #puts "=== extracted '#{$1}' from '#{topaz_garbage}'"
  case str_value
  when /nil/
    nil
  when /\d+/
    str_value.to_i
  else
    str_value
  end
end

# Run the PersistenceHelper in a subshell and ensure that it's output is
# equal to +obj+.  If not, then raise an error using +msg+.
def expect(obj, msg)
  actual = extract_return_value `maglev-ruby PersistenceHelper.rb`
  unless actual == obj
    raise "#{msg}: Expecting #{obj.inspect} but got #{actual.inspect}"
  end
end


# The main test
Dir.chdir working_dir do

  # First, ensure the hat is nil (reset from any previous state in the VM)
  $hat = nil
  Gemstone.commitTransaction

  # Now, expect that the other VM will see a nil $hat
  expect nil, "At start"

  # Stuff a value into the hat.  We do NOT start with a beginTransaction,
  #  since that messes things up
  $hat = 42
  Gemstone.commitTransaction

  # Now, check that the other VM sees the numeric value that we see
  expect 42, "Non nil hat"

  # Clean up the hat for the next run (redundant..)
  $hat = nil
  Gemstone.commitTransaction
end

