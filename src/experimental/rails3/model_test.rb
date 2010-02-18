require 'rubygems'
gem 'i18n'

require 'active_model'

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
