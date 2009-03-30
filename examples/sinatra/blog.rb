puts "=== READING blog.rb"
class Blog < Array
  attr_reader :name

  def initialize(name)
    @name = name
  end

  def add_post(a_post)
    self << a_post
  end
end

