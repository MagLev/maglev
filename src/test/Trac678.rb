class Txx
  class << self
    # Load Property options
    property_option = :accessor678
    self.class_eval <<-RUBY, __FILE__, __LINE__ + 1
      def #{property_option}(*args)
      end
    RUBY
  end
end
cls = Txx
unless cls.respond_to?( :accessor678 ) ; raise 'error'; end
true

