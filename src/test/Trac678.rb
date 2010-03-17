class Type
  class << self
    # Load Property options
    property_option = :accessor
    self.class_eval <<-RUBY, __FILE__, __LINE__ + 1
      def #{property_option}(*args)
      end
    RUBY
  end
end

