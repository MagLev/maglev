
r = 'yml'.tr('.', '')
raise "Fail 1: got #{r.inspect} expecting: yml" unless r == 'yml'

r = 'yml'.tr('m', '')
raise "Fail 2: got #{r.inspect} expecting: yl" unless r == 'yl'
#################### Trac Info
# ID:         486
# Summary:    Problem with string translation
# Changetime: 2009-04-28 00:08:31+00:00
###

#  See attached stack frames