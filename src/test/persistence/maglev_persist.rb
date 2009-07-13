# Specification for Class#maglev_persist!

describe "from features defined by class Foo...end, the first time" do
  # When we say "stages" we mean that everything is set to
  # persist the item, but that it is not yet persisted.
  it "Stages receiver's constants"
  it "Stages receiver's class intance variables"
  it "Stages receiver's class variables"
  it "Stages receiver's instance methods"
  it "Stages receiver's class methods"
  it "Stages receiver's mixed-in module list"
  it "Stages receiver's name as a constant in parent scope"
  it "Commits all staged items with Maglev.commit_transaction"
  it "Clears all staged items with Maglev.abort_transaction"
  it "Stages all items from previous openings of reciever"
  it "Stages reciever's eigenclass"
  it "Returns receiver upon successful completion"
end

describe "from features defined by metaprogramming" do
  it "works with features defined in standard class Foo ... end"
  it "works with features defined via define_method"
  it "works with features defined via .attr_*"
  it "works with features defined via instance_eval"
  it "works with features defined via class_eval"
  it "works with features defined via remove_const"
  it "works with features defined via remove_method"
  it "works with features defined via undef_method"
  it "works with features defined via remove_class_variable"
  it "works with features defined via module_function"
  it "works with features defined via include"
  it "works with features defined via extend_object"
  it "works with features defined via alias_method"
  it "works with features defined via module_eval"
  it "works with features defined via const_set"
end

describe "Error conditions" do
  it "Raises NonPersistentAncestorException if a non-persistent module is mixed in."
  it "Raises NonPersistentNamespaceException if the parent namespace is not persistent"
  it "Does not raise an exception if the parent namespace is not persistent but class is anonymous"
  it "Raises AlreadyPersistentException if maglev_persist! already called"
end

describe "coordinates with maglev_persist?" do
  it "returns false on non_persistent classes"
  it "returns true after maglev_persist! has been called and committed"
  it "returns true after maglev_persist! but transaction has not been committed"
end

describe "when an unhandled exception occurs" do
  it "does not commit the state of the class to the repository"
  it "poisons the transaction (will not allow commit_transaction)"
  it "abort_transaction clears poisoned transaction state"
end


# Variant on Array

describe "Array version" do
  it "Raises NonPersistentClassException if a non-persistent ancestor of an element is not in the array"
  it "Does not raise an exception if all non-persistent ancestors are listed in the array"
  it "Raises an exception if an element of the array is already persistent"
  it "Does not raise an exception if an element of the array has maglev_persist! called twice during processing"
end
