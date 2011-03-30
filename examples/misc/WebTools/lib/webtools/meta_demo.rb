require 'rubygems'
require 'active_model'

class AValidPerson
  include ActiveModel::Validations
 
  validates_presence_of :first_name, :last_name
 
  attr_accessor :first_name, :last_name
  def initialize(first_name, last_name)
    @first_name, @last_name = first_name, last_name
  end
end
