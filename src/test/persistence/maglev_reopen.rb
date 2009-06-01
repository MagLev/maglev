# Specification for Class#maglev_persist!

describe "when called on a persistent class" do
  it "Stages all changes to receiver done in the block"
  it "does not stage recievers changes from previous re-openings not marked by maglev_reopen!"
  it "Raises NonPersistentAncestorException if a non-persistent module was mixed in during the block."
  it "Does not stage a mixed in, non-persistent module from a previous openeing."
end

describe "when called on a non persistent class" do
  it "raises NonPersistentClassException"
end

# Array variant

desc "Array version" do
  it "Raises an exception if any of the elements of the array are not persistent"
  it "Does not commit any changed classes that are not in the list"
end
