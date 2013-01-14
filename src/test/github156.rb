# https://github.com/MagLev/maglev/issues/156
#
# A subsequent autoload should override the previous autoload.

autoload :Github156Autoload, File.dirname(__FILE__) + '/test_data/InvalidAutoload'
autoload :Github156Autoload, File.dirname(__FILE__) + '/test_data/ValidAutoload'

raise "Github issue 156: autoload behavior differs from MRI" unless Github156Autoload::evaluate
