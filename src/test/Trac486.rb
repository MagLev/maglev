
r = 'yml'.tr('.', '')
raise "Fail 1: got #{r.inspect} expecting: yml" unless r == 'yml'

r = 'yml'.tr('m', '')
raise "Fail 2: got #{r.inspect} expecting: yl" unless r == 'yl'
