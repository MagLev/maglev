# Load MagLev FFI support, then load pure ruby portion of psych.
$: << "#{File.dirname(__FILE__)}/psych"

# First load FFI code
require 'psych/ffi/psych'

# Then load Psych code
require 'psych/psych.rb'

module Psych
  class << self
    alias :each_document :load_documents
  end
end
