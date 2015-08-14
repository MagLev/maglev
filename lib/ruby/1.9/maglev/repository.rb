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
    primitive_nobridge 'list_instances', 'listInstances:'

    # Writes a full backup file containing the most recently committed
    # version of the receiver as of the time the method is executed.
    #
    # This method is mapped to fullBackupCompressedTo: which causes the
    # backup file to be compressed as it is written.
    #
    # If the fileName is does not end in '.gz' then a '.gz' suffix is added.
    #
    # The fileName argument may use GemStone Network Resource String syntax.
    # For example, this may be used to access a file on another machine, provided
    # a GemStone NetLDI process is running on the remote machine.
    #
    # If the device containing the file runs out of space, then the backup
    # terminates with a system I/O error and the partially written backup
    # file is deleted.
    #
    # If the session contains uncommitted changes to the repository, the method
    # signals a error: #rtErrAbortWouldLoseData, to indicate that data could
    # be lost.  Otherwise, it puts the session in autoBegin mode and performs
    # the backup operation.  The session may be aborted a number of times during
    # the backup to prevent a commit record backlog.
    #
    # When the backup completes, the session is set to manualBegin mode
    # mode so that it does not reference a commit record  that would cause
    # the repository to grow.
    #
    # Returns true if the backup was completed.
    #
    # This method requires the FileControl privilege.
    #
    # This method performs the backup using multiple slave sessions.
    # The number of slave sessions is automatically determined from the
    # number of extents in the repository, with a minimum of 2 sessions and
    # a maximum of 16.  The performance can be modified during the run by
    # updating the Multithreaded Scan Tuning methods.
    #
    # A GciHardBreak during this method will terminate the session.
    primitive_nobridge 'full_backup_to', 'fullBackupCompressedTo:'


    # Disables logins and starts a full restore of the repository based
    # on the contents of the specified backup file.

    # Restored objects are clustered in a manner that is similar, but not
    # necessarily identical to the clustering at the time the  backup file
    # was created.  If the Repository being restored into has the same
    # number of extents as the system had when the backup file was created,
    # then distribution of objects within extents is preserved unless one
    # of the extents becomes full.  If the number of extents is different
    # than the number when the backup was created, then the current
    # DBF_ALLOCATION_MODE configuration controls the distribution of
    # objects across the extents.

    # If the backup file was made when in partial-logging mode, then logins
    # are reenabled and the system is fully operational when the restore
    # completes.  If the backup was made when in full-logging mode, then
    # the system is not fully operational until tranlogs have been
    # restored and the commitRestore method is executed.

    # If the backup file was created with the compress: argument true or
    # if the file was compressed using 'gunzip', then the input file should
    # be specified with the '.gz' suffix.

    # To minimize the size of the resulting repository the stone should be
    # restarted on a copy of the the initial repository (copied from
    # $GEMSTONE/bin/extent0.dbf).

    # To optimize the time to restore the backup, the exents in the new
    # repository should be pregrown to the minimum expected size of
    # the restored repository.

    # Upon successful completion, the session is automatically logged out and
    # the RestoreBackupSuccess error (4046) is generated.

    # A GciHardBreak during this method will terminate the session.

    # This method requires the FileControl privilege.  It is recommended
    # that it be run by either DataCurator or SystemUser.

    # This method performs the restore using multiple slave sessions.
    # The number of slave sessions is automatically determined from the
    # number of extents in the repository, with a minimum of 2 sessions and
    # a maximum of 16.  The performance can be modified during the run by
    # updating the Multithreaded Scan Tuning methods.
    primitive_nobridge 'restore_from_backup', 'restoreFromBackup:'
  end
end
