# Maglev.persistent do
  module ObjectSpace
    # SystemRepository is the singleton instance of class Repository
    SystemRepository = __resolve_smalltalk_global(:SystemRepository)

    class Repository
      primitive_nobridge '__list_instances', 'listInstances:'

      # Returns a list of instances in the repository that belong to one of
      # the classes listed in an_array.  The result of this method is an
      # Array of Arrays, where the contents of each inner array consists of
      # all instances whose class is equal to the corresponding element in
      # an_array.
      #
      # This method aborts the current transaction; if an abort would cause
      # unsaved changes to be lost, it raises an exception.
      #
      # If an_array contains multiple occurrences of a class, then the
      # result will contain corresponding multiple occurrences of the same
      # Array that lists the instances of that class.
      #
      # If an_array contains an element that is not a class or module (kind
      # of Behavior), an error is generated.
      #
      # Scans the entire Repository at least once.
      #
      # If the argument an_array contains more than 2000 unique elements
      # then the entire Repository will be scanned once for each group of
      # 2000 unique elements, or fraction thereof.
      def list_instances(an_array)
        SystemRepository.__list_instances(an_array)
      end
    end
  end

  class Class
    # Searches the entire MagLev repository for all instances of receiver
    # (this could take a while).  This method aborts the current
    # transaction; if an abort would cause unsaved changes to be lost, it
    # raises an exception.  See ObjectSpace::Repository#list_instances for
    # more details.
    def all_instances
      ObjectSpace::SystemRepository.list_instances([self])[0]
    end
  end

  class Object
    # Swaps the identities of the receiver and the argument.
    #
    # Intended only for experienced MagLev programmers who need to migrate
    # instances of one class to another.
    #
    # The sender is responsible for checking the consistency of the class
    # histories of the argument and the receiver.  This method makes no
    # such checks (not applicable to MagLev).
    #
    # The argument, the receiver, or both are permitted to be invariant.
    #
    # Neither the argument nor the receiver may be special objects (instances of
    # classes such as SmallInteger, Character, or Boolean).  Also, neither may be
    # instances of a class that is a kind of
    #
    #    GsProcess, VariableContext, BlockClosure, GsSocket, GsFile, GsNMethod,
    #    CLibrary, CFunction, CPointer, CByteArray, Regexp, or GsCompilerNode.
    #
    # Neither the argument nor the receiver may be a kind of Bag that has
    # indexes built on it.  If either the receiver or the argument (or
    # both) participate in an index, then either both must be in byte
    # format or neither must be in byte format.  That is, one cannot be in
    # byte format if the other is not also.  To avoid the error conditions
    # triggered by presence of indexes, remove the indexes from the
    # relevant NSCs prior to invoking this method.
    #
    # Neither the argument nor the receiver may exist as self below the
    # sender of a become: message on the active MagLev stack.
    #
    # Once the identities have been swapped, the argument and receiver may
    # no longer satisfy the constraints of objects that reference them.
    # This condition can lead to the failure of subsequent index creation
    # attempts.  The MagLev programmer is responsible for correcting broken
    # constraints.
    #
    # Any clusterIds that belong to an object on disk remain with the
    # object.  That is, the clusterIds do not follow the identities when
    # they are swapped.
    #
    # The ObjectSecurityPolicies of the objects do not follow the
    # identities when they are swapped.
    #
    # As of Gs64 v3.0, tags are no longer swapped between the objects, they
    # are treated same as instance variables.
    #
    primitive_nobridge 'become', 'become:'

    # primitive_nobridge 'become', '_becomeMinimalChecks:'
  end
# end

# Maglev.commit_transaction
