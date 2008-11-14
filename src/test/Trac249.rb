# The main test case for ticket 249:
class Foo
  module Constants
    BAR = "bar"
  end
  include Constants

  puts BAR
end

puts Foo::BAR


# A secondary case is due to inheritance:

class Foo
  FOO = 'foo'
end

class Bar < Foo
end

puts Bar::FOO  # Blows up

# IO.rb and File.rb also depend on this behavior working, so here are some
# tests so we don't miss them too.
#
# TODO: Update comments and TODOs in IO.rb and File.rb when this is fixed.

require File.expand_path('simple', File.dirname(__FILE__))

test(IO::FNM_SYSCASE, 0, 'IO::FNM_SYSCASE')           # defined in IO.rb
test(File::FNM_SYSCASE, 0, 'File::FNM_SYSCASE')       # Inherited from IO
test(File::PATH_SEPARATOR, ':', 'File::PATH_SEPARATOR') # defined in File
test(File::ALT_SEPARATOR, nil, 'File::ALT_SEPARATOR')   # mixed into File

report
