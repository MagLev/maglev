# Found in Rails 3.1
#
# Assume lib/trac922_helper.rb file is:
#
#   # lib/trac922_helper.rb
#   module Sprockets
#     module Helpers
#       module RailsHelper
#         def rails_helper
#           :foo
#         end
#       end
#     end
#   end
#
# Then MRI is fine, but MagLev has following error:
#
#   $ maglev-ruby sprocket.rb
#   -- RubyFile>>load  : loading /Users/pmclain/tmp/sprocket.rb
#   error , a ArgumentTypeError occurred (error 2719), left side of :: is neither a class nor module,
#                during /Users/pmclain/tmp/sprocket.rb
#   ERROR 2719 , a ArgumentTypeError occurred (error 2719), left side of :: is neither a class nor module (TypeError)
#   topaz 1>

$: << File.dirname(__FILE__)  # Make sure lib/trac922_helper.rb will be on load path

module Sprockets
  autoload :Helpers, "lib/trac922_helper"

  class Railtie
    def self.config
      yield self
    end
    config do |app|
      # Helpers should be autoloaded, and will define RailsHelper
      include ::Sprockets::Helpers::RailsHelper
    end
  end
end
