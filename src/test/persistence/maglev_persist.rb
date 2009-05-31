# Specification for Class#maglev_persist!

describe "the first time" do
  # When we say "stages" we mean that everything is set to
  # persist the item, but that it is not yet persisted.
  it "Stages its constants"
  it "Stages its class intance variables"
  it "Stages its class variables"
  it "Stages its instance methods"
  it "Stages its class methods"
  it "Stages its name as a constant in Object"
  it "Commits all staged items with Maglev.commit_transaction"
  it "Clears all staged items with Maglev.abort_transaction"
  it "Stages all items from previous openings of reciever"
end

describe "after the first time" do
  it "raises an exception if Class#maglev_reopen has not been called since maglev_persist! was called"
  it "Stages only items defined or changed since maglev_reopen was called."
  it ""
end

describe "Classes within a module" do
  it "raises an exception if you persist a class in a non-persistant module"
end

# define_const
# set_const or whatever
# some classes in a module are persistent and others not
# All globals, including $LOADED_FEATURES are VM local
