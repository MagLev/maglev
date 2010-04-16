Maglev.persistent do
  class Data
    ALL_DATA = nil
    attr_reader :age, :weight, :length
    def initialize(age, weight, length)
      @age = age
      @weight = weight
      @length = length
    end
  end
  Data::ALL_DATA = IdentitySet.new
  Data::ALL_DATA.create_identity_index('@age')
  Data::ALL_DATA.create_identity_index('@weight')
  # Don't do length.... just to test efficiency
end
Maglev.commit_transaction

10.times do 
  10_000.times do |i|
    Data::ALL_DATA << Data.new(rand(100), rand(100), rand(100))
  end
  puts "Committing 10_000 items: total: #{Data::ALL_DATA.size}"
  Maglev.commit_transaction
end
puts "Committed indexed data"

