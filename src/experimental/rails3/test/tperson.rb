# Rails has a Person and person.rb, so rename to avoid buggy clash
class TPerson
  include ActiveModel::Validations

  validates_presence_of :first_name, :last_name

  validates_each :first_name, :last_name do |record, attr, value|
    record.errors.add attr, 'starts with z.' if value.to_s[0] == ?z
  end

  attr_accessor :first_name, :last_name
  def initialize(first_name, last_name)
    @first_name, @last_name = first_name, last_name
  end
end
