class Person
  attr_reader :name, :friends, :favorite_color
  def initialize(name, fav_color=nil)
    @name = name
    @friends = IdentitySet.new
    @favorite_color = fav_color
  end

  def add_friend(person)
    @friends.add person
  end
  def remove_friend(person)
    @friends.remove person
  end
  def to_s
    acc = ''
    @friends.each {|f| acc << "#{f.name}, " }
    "#{@name} #{@favorite_color}  #{acc}"
  end
end


