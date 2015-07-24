module Maglev
  Repository = __resolve_smalltalk_global(:Repository)
  class Repository

    # Returns the current repository (equivalent to the Smalltalk global
    # SystemRepository).
    def self.instance
      __resolve_smalltalk_global(:SystemRepository)
    end

    # Usage: Repository.instance.list_instances(array_of_classes).
    #
    # Returns a list of instances on the receiver that belong to one of the
    # classes listed in array_of_classes.  The result of this method is an
    # Array of Arrays, where the contents of each inner array consists of
    # all instances whose class is equal to the corresponding element in
    # array_of_classes.
    #
    # This method aborts the current transaction; if an abort would cause
    # unsaved changes to be lost, it signals an error,
    # #rtErrAbortWouldLoseData.
    #
    # If array_of_classes contains multiple occurrences of a class, then
    # the result will contain corresponding multiple occurrences of the
    # same Array that lists the instances of that class.
    #
    # If array_of_classes contains an element that is not a Class or
    # Module, an error is generated.
    #
    # Scans the entire Repository at least once.
    #
    # If the argument array_of_classes contains more than 2000 unique
    # elements then the entire Repository will be scanned once for each
    # group of 2000 unique elements, or fraction thereof.
    #
    primitive_nobridge 'full_backup_to', 'fullBackupCompressedTo:'
    primitive_nobridge 'list_instances', 'listInstances:'
    primitive_nobridge 'restore_from_backup', 'restoreFromBackup:'
  end
end
