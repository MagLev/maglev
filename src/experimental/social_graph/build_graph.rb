Maglev.persistent do
  load 'graph.rb'
end
Maglev::PERSISTENT_ROOT[:Person] = IdentitySet.new
Maglev.commit_transaction

require '../../../examples/persistence/names.rb'

root = Maglev::PERSISTENT_ROOT[:Person]

num_people = 1_000_000
friends_per_person = 50
chunk_size = 1_000

puts "=== Building #{num_people} people with #{friends_per_person} friends each"
colors = [:red, :blue, :yellow, :blue_no_red]

people = []
(num_people / chunk_size).times do |i|
  puts "--[#{i}] Generating #{chunk_size} people..."
  chunk_size.times do |j|
    gender = rand(2) == 0 ? :male : :female
    name = RandomNameGenerator.name_for(gender)
    p = Person.new(name, colors.at(rand(4)))
    people << p # temporary, so we can get names
    root << p
  end
  Maglev.commit_transaction
end


puts "-- Adding friends..."
current = 0
root.each do |p|
  current += 1
  if current % chunk_size == 0
    puts "commit #{current}"
    Maglev.commit_transaction
  end
  friends_per_person.times do |i|
    p.add_friend(people[rand(num_people)])
  end
end
Maglev.commit_transaction

