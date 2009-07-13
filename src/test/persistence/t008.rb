# Define a class
class Foo
   # stuff
end

# Mark the class as persistable
Foo.maglev_persistable = true

# Create a space in PERSISTENT_ROOT to hold persistent Foo objects:
Maglev::PERSISTENT_ROOT[:my_favorite_foos] = Array.new

# Connect a Foo instance to a persistent root
Maglev::PERSISTENT_ROOT[:my_favorite_foos] << Foo.new

Maglev.commit_transaction  # commit class Foo and the instance f.

