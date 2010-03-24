class TranslatePerson
  extend ActiveModel::Translation

  attr_accessor :first_name, :last_name
  def initialize(first_name, last_name)
    @first_name, @last_name = first_name, last_name
  end
end
