# Define a class
class Foo456
   # stuff
end

# Mark the class as persistable
Foo456.maglev_persistable

# Create a space in PERSISTENT_ROOT to hold persistent Foo objects:
Maglev::PERSISTENT_ROOT[:my_favorite_foos] = Array.new

# Connect a Foo instance to a persistent root
Maglev::PERSISTENT_ROOT[:my_favorite_foos] << Foo456.new

Maglev.commit_transaction  # commit class Foo and the instance f.

