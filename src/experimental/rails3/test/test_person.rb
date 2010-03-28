class TestPerson
  include ActiveModel::AttributeMethods

  attribute_method_affix :prefix => 'reset_', :suffix => '_do_default!'
  attribute_method_suffix '_contrived?'
  attribute_method_prefix 'clear_'
  define_attribute_methods ['name']

  attr_accessor :name

  private

  def attribute_contrived?
    true
  end
  def clear_attribute(attr)
    send("#{attr}=", nil)
  end

  def reset_attribute_to_default!(attr)
    send("#{attr}=", "Default Name")
  end
end
