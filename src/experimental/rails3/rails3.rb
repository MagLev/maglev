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


class Person
  include ActiveModel::Validations

  validates_presence_of :first_name, :last_name

  attr_accessor :first_name, :last_name
  def initialize(first_name, last_name)
    @first_name, @last_name = first_name, last_name
  end
end

p1 = Person.new('peter', 'mclain')
p p1
