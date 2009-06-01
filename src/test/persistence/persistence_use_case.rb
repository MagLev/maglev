# This is a sample use case for the proposed MagLev persistence API.


########################################################################
#    Step 1: Create a class and persist it.
########################################################################

class MyClass
  @class_instance_variable = 1
  PERSISTENT_CONSTANT = 10
  def meth1
    "original meth1"
  end
  def meth2
    "original meth2"
  end
end

MyClass.maglev_persist!

# At this point, the class has been marked persistable, but the changes
# have not yet been saved in the repository:
#
#  |--------------------------+------------------+-----------------|
#  | Feature                  | Local VM view    | Repository view |
#  |--------------------------+------------------+-----------------|
#  | @class_instance_variable | 1                | N/A             |
#  | PERSISTENT_CONSTANT      | 10               | N/A             |
#  | meth1                    | "original meth1" | N/A             |
#  | meth2                    | "original meth2" | N/A             |
#  |--------------------------+------------------+-----------------|

Maglev.commit_transaction

# Now, the current VM and the repository have the same view of the class:
#
#  |--------------------------+------------------+------------------|
#  | Feature                  | Local VM view    | Repository view  |
#  |--------------------------+------------------+------------------|
#  | @class_instance_variable | 1                | 1                |
#  | PERSISTENT_CONSTANT      | 10               | 10               |
#  | meth1                    | "original meth1" | "original meth1" |
#  | meth2                    | "original meth2" | "original meth2" |
#  |--------------------------+------------------+------------------|

########################################################################
#    Step 2: transient modifications
########################################################################

class MyClass
  @class_instance_variable = 2
  def meth1
    "transient meth1"
  end
  def meth3
    "transient meth3"
  end
  TRANSIENT_CONSTANT = :foo
end

# The current VM session will now always see a different view of MyClass
# than the repository view.  None of the changes made during step 2 are
# visible outside of the current VM session.  Note especially the '**"
# rows:
#
#  |--------------------------+-------------------+------------------|
#  | Feature                  | Local VM view     | Repository view  |
#  |--------------------------+-------------------+------------------|
#  | @class_instance_variable | 2                 | 1                | **
#  | PERSISTENT_CONSTANT      | 10                | 10               |
#  | meth1                    | "transient meth1" | "original meth1" | **
#  | meth2                    | "original meth2"  | "original meth2" |
#  | meth3                    | "transient meth3" | N/A              | **
#  | TRANSIENT_CONSTANT       | :foo              | N/A              | **
#  |--------------------------+-------------------+------------------|


Maglev.commit_transaction

# Since the chages were not made in a reopen block, the Local VM view and
# Repository view remain the same:
#
#  |--------------------------+-------------------+------------------|
#  | Feature                  | Local VM view     | Repository view  |
#  |--------------------------+-------------------+------------------|
#  | @class_instance_variable | 2                 | 1                |
#  | PERSISTENT_CONSTANT      | 10                | 10               |
#  | meth1                    | "transient meth1" | "original meth1" |
#  | meth2                    | "original meth2"  | "original meth2" |
#  | meth3                    | "transient meth3" | N/A              |
#  | TRANSIENT_CONSTANT       | :foo              | N/A              |
#  |--------------------------+-------------------+------------------|


########################################################################
#    Step 3: persistent modifications
########################################################################

MyClass.maglev_reopen! do

  # modify the class
  class MyClass
    def meth1
      "Step 3 meth1"
    end
  end

  # can do other things
  require 'foo'  # who knows what happens in foo....

  # Class foo is not mentioned as the receiver of maglev_reopen!,
  # so no changes to Foo will become persistent
  class Foo
    # ...
  end

  # and reopen the class again
  class MyClass
    def meth4
      "Step 3 meth4"
    end
  end
end

# The view before committing shows the staged changes:
#
#  |--------------------------+-------------------+------------------|
#  | Feature                  | Local VM view     | Repository view  |
#  |--------------------------+-------------------+------------------|
#  | @class_instance_variable | 2                 | 1                |
#  | PERSISTENT_CONSTANT      | 10                | 10               |
#  | meth1                    | "Step 3 meth1"    | "original meth1" | **
#  | meth2                    | "original meth2"  | "original meth2" |
#  | meth3                    | "transient meth3" | N/A              |
#  | TRANSIENT_CONSTANT       | :foo              | N/A              |
#  | meth4                    | "Step 3 meth4"    | N/A              | **
#  |--------------------------+-------------------+------------------|

Maglev.commit_transaction

# At this point, the current session and the repository have the same
# view of of the persistent features, and the local version has two
# features not in the repository (`TRANSIENT_CONSTANT` and `meth3`).
#
#  |--------------------------+-------------------+------------------|
#  | Feature                  | Local VM view     | Repository view  |
#  |--------------------------+-------------------+------------------|
#  | @class_instance_variable | 2                 | 1                |
#  | PERSISTENT_CONSTANT      | 10                | 10               |
#  | meth1                    | "Step 3 meth1"    | "Step 3 meth1"   |
#  | meth2                    | "original meth2"  | "original meth2" |
#  | meth3                    | "transient meth3" | N/A              |
#  | TRANSIENT_CONSTANT       | :foo              | N/A              |
#  | meth4                    | "Step 3 meth4"    | "Step 3 meth4"   |
#  |--------------------------+-------------------+------------------|
