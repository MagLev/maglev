libdir = File.expand_path(File.join(File.dirname(__FILE__), '../../../'))
$LOAD_PATH.unshift libdir
require 'vendor/bacon'
require 'ramaze/spec/helper/pretty_output'
Bacon.extend Bacon::PrettyOutput
# Bacon.extend Bacon::TestUnitOutput
Bacon.summary_on_exit
