require File.expand_path('simple', File.dirname(__FILE__))

idset = IdentitySet.new
idset.create_index('id', Fixnum)

class Id
  def initialize(id)
    @id = id
  end
end

class Bar
  def initialize(id)
    @id = id
  end
end

10.times { |i| idset << Id.new(i) }
test(idset.length, 10, 'Inserted 10 Id instances')

10.times { |i| idset << Bar.new(i) }
test(idset.length, 20, 'Inserted 10 Bar instances')

old_ones = idset.select([:id], :>=, 7)
test(old_ones.length, 6, 'Select 1 worked')

report




