require 'person'

people = IdentitySet.new

people.create_equality_index(':@age', Fixnum)
people.create_identity_index('@gender')
people.create_equality_index('@address.@zip', Fixnum)

1000.times { people << Person.random }

males = people.search([:@gender], :equal, :male)
age_group = people.search_range([:@age], (18..25))
in_zipcode = people.search([:@address, :@zip], :equal, 45678)
lucrative_market = males & age_group & in_zipcode

p lucrative_market
p lucrative_market.length

people.remove_all_indexes
