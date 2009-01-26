#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'nitro/helper/form'

module Ramaze
  module Helper

    # This helper simply includes the Nitro::FormHelper so you can use its methods
    # in your Controller.

    Nitroform = ::Nitro::FormHelper
  end
end
