# Step 1:  First opening of the class
Maglev.persistent do
  class C007
    A_CONST=1
  end
end

Maglev.commit_transaction   # commit the class

# Step 2
#
# This re-opening of the class is not within the scope of a call to
# Maglev.persistent, so none of these changes will be staged for
# persistence.  The current VM will be the only VM to see
# A_NON_PERSISTENT_CONST, a_non_persistent_method and
# an_ambiguous_method.
Maglev.transient do
  class C007
    A_NON_PERSISTENT_CONST = 42
    def a_non_persistent_method
    end

    def an_ambiguous_method
    end

  end
end

# Step 3
#
# This re-opening of the class *is* within the scope of a call to
# Maglev.persistent, so all of these changes will be staged for
# persistence.  This will stage A_SECOND_PERSISTENT_CONST,
# a_persistent_method, and an_ambiguous_method persistent for
# persistence, but will NOT stage the other items from Step 2 for
# persistence (i.e. A_NON_PERSISTENT_CONST and a_non_persistent_method
# are still local to the VM and non-persistent; an_ambiguous_method
# becomes persistent, with the definition from step 3).

Maglev.persistent do
  class C007
    A_SECOND_PERSISTENT_CONST = 53

    def a_persistent_method
    end

    def an_ambiguous_method
    end
  end
end

Maglev.commit_transaction
