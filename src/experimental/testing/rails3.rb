# This file includes the Rails 3 version of active support
#
# git://github.com/rails/rails.git
require 'rubygems'
gem 'i18n'

$:.unshift "/Users/pmclain/external/rails/activesupport/lib"
require 'active_support'

$:.unshift "/Users/pmclain/external/rails/activemodel/lib"
require 'active_model'


#   i18n is the problem here
$:.unshift "/Users/pmclain/external/rails/activeresource/lib"
require 'active_resource'

