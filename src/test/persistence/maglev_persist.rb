# Specification for Class#maglev_persist!

describe "the first time, with include_all=true" do
  # When we say "stages" we mean that everything is set to
  # persist the item, but that it is not yet persisted.
  it "Stages receiver's constants"
  it "Stages receiver's class intance variables"
  it "Stages receiver's class variables"
  it "Stages receiver's instance methods"
  it "Stages receiver's class methods"
  it "Stages receiver's name as a constant in parent scope"
  it "Commits all staged items with Maglev.commit_transaction"
  it "Clears all staged items with Maglev.abort_transaction"
  it "Stages all items from previous openings of reciever"
  it "Stages reciever's eigenclass"

  # Note: recursive application of maglev_persist will catch the closure of
  # unpersisted modules etc.
  it "Calls maglev_persist! on its containing module if non-persistent"
  it "Calls maglev_persist! on its superclass if non-persistent"
  it "Calls maglev_persist! on all non-persistent mixed in modules"
end

describe "after the first time" do
  it "raises AlreadyPersistentException on subsequent calls"
  it "Stages only items defined or changed since maglev_reopen was called."
  it ""
end

describe "maglev_persist? works" do
  it "returns false on non_persistent classes"
  it "returns true after maglev_persist! has been called and committed"
  it "returns ??? after maglev_persist! but transaction has not been persisted?"
end

describe "Classes within a module and include_all=false" do
  it "raises an exception if you persist a class in a non-persistant module"
end

# Questions
#
#    What happens when a persistent class mixes in a non-persistent module?
#    should it only persist it if maglev_reopen! was called ?

# define_const
# set_const or whatever
# some classes in a module are persistent and others not
# All globals, including $LOADED_FEATURES are VM local
