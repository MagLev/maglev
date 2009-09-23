module JSON
  # The bug only seems to happen if you toggle the module_function state
  # this way.  See comment below.
  module_function

  # This method works fine: no args
  def foo
    puts "foo"
  end

  # This method doesn't work
  def parse(source, opts = { })
    puts "#{self}.parse"
  end

  # If you explicitly call module_function like below, then it will work
  # module_function :parse
end

JSON.foo            # OK
JSON.parse :x       # Fails
