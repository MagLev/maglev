#  MagLev uses the ruby code from Psych, plus our own FFI wrapper, to
#  implement YAML.
#
$: << "#{File.dirname(__FILE__)}/psych"

# First load FFI code
require 'psych/ffi/psych'

# Then load Psych code
require 'psych/psych.rb'

YAML = Psych
