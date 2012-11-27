# Some tests that ensure MagLev does correct stuff when invoked in various ways
#
require File.expand_path('simple', File.dirname(__FILE__))

# this is so we can run this file with either MRI or MagLev
A_OUT = defined?(RUBY_ENGINE) ? 'maglev-ruby' : 'ruby'
ENV['MAGLEV_OPTS'] = ''  # Ensure -MtraceLoad doesn't mess us up

# ############################################################
# Ensure __FILE__ is correct
# ############################################################
Struct.new("TC", :pwd, :prefix, :result) do
  def to_s
    "#{pwd} => #{prefix}"
  end
end

here = Dir.pwd + '/src/test'
test_cases =  [
  Struct::TC.new(Dir.pwd, "./src/test/lib/", "./src/test/lib/echo__FILE__.rb\n"),
  Struct::TC.new(here + '/lib', "", "echo__FILE__.rb\n"),
  Struct::TC.new(here + '/lib', "./", "./echo__FILE__.rb\n"),
  Struct::TC.new(here, 'lib/', "lib/echo__FILE__.rb\n"),
  Struct::TC.new(here, "#{here}/lib/", "#{here}/lib/echo__FILE__.rb\n"),
  # For the next line to pass do this in the command line: 
  # sudo chmod o+w /
  # because it needs read and write in / 
  Struct::TC.new('/',  "#{here[1..-1]}/lib/", "#{here[1..-1]}/lib/echo__FILE__.rb\n"),
]

test_cases.each_with_index do |tc,i|
  Dir.chdir(tc.pwd) do
    test(`#{A_OUT} #{tc.prefix}echo__FILE__.rb`, tc.result, "[#{i}] #{tc}")
  end
end

# Make sure if file does not end in .rb, that __FILE__ does not end in .rb
test(`#{A_OUT} #{here}/lib/echo__FILE__`, "#{here}/lib/echo__FILE__\n", 'no .rb')

# ############################################################
# Ensure -Mcommit and -Mpersistent start in the correct mode
# ############################################################
test(`#{A_OUT} -e 'p Maglev.persistent?'`, "false\n", 'default is transient mode')
test(`#{A_OUT} -e 'p Maglev.transient?'`, "true\n",   'default is transient mode 2')

test(`#{A_OUT} -Mpersistent -e 'p Maglev.persistent?'`, "true\n", '-Mpersistent')
test(`#{A_OUT} -Mcommit -e 'p Maglev.persistent?'`, "true\n", '-Mcommit')
test(`#{A_OUT} -Mpersistent -Mcommit -e 'p Maglev.persistent?'`, "true\n", '-Mcommit -Mpersistent')


# Ensure -Mcommit and -Mpersistent start in the correct mode

# Test -I works for -e
# test(`#{A_OUT} -Ilib -e 'require "bar"' -e 'p $a_value'`, "987\n", '-I -e')


report
