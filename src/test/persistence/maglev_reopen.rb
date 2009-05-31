# Specification for Class#maglev_persist!

describe "when called on a persistent class" do
  it "Stages recievers changes in the next reopening of the class"
  it "does not stage recievers changes from previous re-openings not marked by maglev_reopen!"
  it "is turned off after the closing end of the next reopening."
  it "is turned off at the end of the file????"
end


describe "when called in one file, but effect is in another file" do
  # These expectations cover the case of when maglev_reopen! is called
  # in file A, but file A does not subsequently reopen the class.

  # E.g.:
  #    # File A
  #    MyClass.maglev_reopen!
  #    require "fileB.rb"      # opens MyClass
  it "is effective in required files"

  # E.g., if fileA.rb contains:
  #
  #    # File A
  #    class MyClass
  #    end
  #    MyClass.maglev_persist!
  #
  #    MyClass.maglev_reopen!
  #    # End of file A
  #
  # and fileB.rb contains:
  #
  #    # File B
  #    class MyClass
  #      # does something
  #    end
  #
  # And in fileC.rb, you do:
  #
  #    # File C
  #    load "fileA.rb"
  #    load "fileB.rb"
  #    Maglev.commit_transaction
  it "is turned off at the end of the current file"
end

describe "when called on a non persistent class" do
  it "raises NonPersistentClassException"
end
