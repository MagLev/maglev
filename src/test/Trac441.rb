# Test that Module#include accepts multiple parameters
require File.dirname(__FILE__) + '/simple'

module M1
  def m1
  end
end

module M2
  def m2
  end
end

module M
  include M1, M2
end

# Right now, M.instance_methods returns too much....
test(M.instance_methods.include?('m1'), true, 'm1')
test(M.instance_methods.include?('m2'), true, 'm2')

report
#################### Trac Info
# ID:         441
# Summary:    Module#include has incorrect arguments
# Changetime: 2009-04-20 20:46:14+00:00
###

#  The method Module#include should take a splat argument and iterate over the list of arguments, rather than just a single argumeent.