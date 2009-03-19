require File.expand_path('simple', File.dirname(__FILE__))

require 'erb'

x = 42
template = ERB.new "The value of x is <%= x %>"
test(template.result(binding), "The value of x is 42", "ERB 1")


# This test currently fails, due to Trac 385.  Uncomment when 385 is fixed.
# The test checks that TOPLEVEL_BINDING picks up the ugly variable
# ugly = true
# s = <<RUBY
#   <% if ugly %>
#       UGLY
#   <% else %>
#       NOT UGLY
#   <% end %>
# RUBY
# test(ERB.new(s).result,          "  \n      UGLY\n  \n", 'ERB defaulting to TOPLEVEL_BINDING')
# test(ERB.new(s).result(binding), "  \n      UGLY\n  \n", 'ERB using new binding')

report
