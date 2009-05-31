## Proposed MagLev Persistence API ##
#
# The following code shows proposed API additions for MagLev persistence.

class MaglevError < StandardError; end

# The base class for all MagLev Persistence Errors.
class MaglevPersistenceError < MaglevError; end
class AlreadyPersistentException < MaglevPersistenceError; end
class NonPersistentClassException < MaglevPersistenceError; end

class Module
  # Mark the receiver as a persistable class and persist the current state
  # of the class.  If the <tt>include_all</tt> flag is true, then also
  # ensure that all classes in the super class chain, and their included
  # modules, are marked as persistable.  Once marked persistable, the state
  # of the class (methods, constants, class variables and class instance
  # variables) will be saved at the next <tt>Maglev.commit_transaction</tt>.
  #
  #  class C
  #    # stuff
  #  end
  #  C.maglev_persist!   # Persists all defined aspects of class at this point.
  #
  # Raises +MaglevPersistenceError+ if the <tt>include_all</tt> flag is
  # +false+, and one of receiver's superclasses or mixed in modules is
  # not marked as persistable.
  #
  # Raises +MaglevPersistenceError+ if the class or module that will hold
  # the constant reference to this class is not persistable.  E.g., an
  # exception is raised in the following code if module M is not
  # persistable:
  #
  #   module M
  #      class C
  #      end
  #
  #      C.maglev_persist!  # will raise exception if M is not persistable.
  #   end
  #
  # Eigenclasses (singleton classes) are automatically marked as
  # persistent when the corresponding object is persisted.
  #
  # Implementation note: this will set the np flag on both the class and
  # the metaclass.  Ruby classes will, by default, have the np bit set in
  # their metaclass, preventing accidental saving of a class.
  #
  # Raises AlreadyPersistentException if called a second time (see also
  # maglev_reopen!).
  #
  # TODO: should this return an array of the classes that were modified
  # (had persistable set to false before the call, and now have it set to
  # true)?
  #
  def maglev_persist!(include_all=true)
    raise AlreadyPersistentException if maglev_persist?
    # ...
  end

  # Returns true if <tt>maglev_persist!</tt> has been called on
  # receiver.  Returns false otherwise.  For Eigenclasses, this bit is
  # inferred from the associated class.
  def maglev_persist?
    # ...
  end

  # Reopen a class for persistence.  Marks the class so that the next time
  # it is opened, all changes will be persisted.   All changes to theE.g.,
  #
  #   class MyClass
  #     # First Open
  #     # ...
  #   end
  #   MyClass.maglev_persist!
  #
  #   class MyClass
  #     # Second Open is non persistent
  #   end
  #
  #   MyClass.maglev_reopen!
  #   class MyClass
  #     # Third Open is persistent
  #   end
  #   Maglev.commit_transaction
  #
  # Raises UnpersistentClassException if the class has not yet been
  # persisted (i.e., if maglev_persist? returns false).
  #
  # TODO: Should this take an optional block?  That way, you could have a
  # natural way to mixin other modules, class_eval etc. and make that
  # persistent too.
  #
  # TODO: Should this get turned off at the end of a file?  E.g., if you do
  # this:
  #    # foo.rb
  #    MyClass.maglev_reopen!
  #
  #  should MyClass be in a re-opened state?  Seems like now you have the
  #  possibility of unintended consequences?  Perhaps this is only good for
  #  the current file? (but that means you can't
  #
  def maglev_reopen!
    raise NonPersistentClassException unless maglev_persist?
    # TODO: Should it raise if already re-opened?
    # ...
  end

  # TODO: Do we need/want this?
  def maglev_reopen?
  end

  # TODO: Do we need/want this?
  def maglev_close!
    raise NotReopenedException unless maglev_reopen?
    # TODO: should it raise if not reopened?
  end
end

module Maglev

  # USER_GLOBALS is a persistent Hash table.  It is the root for
  # holding objects the application needs to persist.
  USER_GLOBALS = :TBD

  # Application level transaction API
  module Transaction

    class OutsideOfTransactionException < Exception; end

    # This exception is raised if MagLev is unable to commit the state of
    # the repository.  The details of why the commit failed are contained
    # in the exception.
    class CommitFailedException
      # TODO: De-smalltalk-ify description and API
      #
      # Returns a SymbolDictionary that contains an Association whose key
      # is #commitResult and whose value is one of the following Symbols:
      # #success, #failure, #retryFailure, #commitDisallowed, or #rcFailure
      # .
      #
      # The remaining Associations in the dictionary are used to report the
      # conflicts found.  Each Association's key indicates the kind of
      # conflict detected; its associated value is an Array of OOPs for the
      # objects that are conflicting.  If there are no conflicts for the
      # transaction, the returned SymbolDictionary has no additional
      # Associations.
      #
      # The conflict sets are cleared at the beginning of a commit or abort
      # and therefore may be examined until the next commit, continue or
      # abort.
      #
      # The keys for the conflicts are as follows:
      #
      #     Key                Conflicts
      # Read-Write          StrongReadSet and WriteSetUnion conflicts.
      # Write-Write         WriteSet and WriteSetUnion conflicts.
      # Write-Dependency    WriteSet and DependencyChangeSetUnion conflicts.
      # Write-WriteLock     WriteSet and WriteLockSet conflicts.
      # Write-ReadLock      WriteSet and ReadLockSet conflicts.
      # Rc-Write-Write      Logical write-write conflict on reduced conflict object.
      # WriteWrite_minusRcReadSet  (WriteSet and WriteSetUnion conflicts) - RcReadSet)
      #
      # The Read-Write conflict set has already had RcReadSet subtracted from
      # it.  The Write-Write conflict set does not have RcReadSet subtracted.
      #
      # Beginning with Gemstone64 v1.1 , the WriteSet no longer includes
      # objects newly committed by this transaction.  Thus a conflict between
      # a lock and a newly committed object in prior releases will no longer
      # show up as a conflict.
      #
      # The Write-Dependency conflict set contains objects modified
      # (including DependencyMap operations) in the current transaction that
      # were either added to, removed from, or changed in the DependencyMap
      # by another transaction. Objects in the Write-Dependency conflict set
      # may be in the Write-Write conflict set.
      #
      # Note: You should be sure to disconnect conflict sets before
      # committing to avoid making them persistent.
      def transaction_conflicts
        raise NotImplementedError
      end

      # TODO: Should we have something simple like this to handle the
      # "Note" at the end of the description for transaction_conflicts?
      #
      # Disconnect (ignore) all conflicts
      def disconnect_conflicts
      end
    end

    # Attempts to update the persistent state of the Repository to include
    # changes made by this transaction.
    #
    # If the commit operation succeeds, then this method returns true, and
    # the current transaction's changes, if any, become a part of the
    # persistent Repository.  After the repository update, the session
    # exits the current transaction.  If the transaction mode is
    # :auto_begin (the MagLev default), then the session enters a new
    # transaction.  If the transaction mode is :manual_begin, then the
    # session remains outside of a transaction.
    #
    # If conflicts prevent the repository update, then this method raises a
    # CommitFailedException which contains details of why the commit
    # failed.  Call the transaction_conflicts method to determine the
    # nature of the conflicts.  If the session is outside of a transaction,
    # then this method raises the error OutsideOfTransactionException
    #
    # This method also updates the session's view of GemStone.  If the
    # commit operation succeeds, then all objects in the session's view are
    # consistent with the current state of GemStone.  If the commit fails,
    # then this method retains all the changes that were made to objects
    # within the current transaction.  However, commits made by other
    # sessions are visible to the extent that changes in this transaction
    # do not conflict with them.
    #
    # Returns true if commit was read-only or succeeded.  Raises
    # OutsideOfTransactionException if there was a failure.
    def commit_transaction
      # TODO: wrap #rtErrPrimOutsideTrans in OutsideOfTransactionException
      unless Gemstone.commitTransaction
        raise CommitFailedException
      end
      return true
    end

    # Rolls back all modifications made to committed GemStone objects and
    # provides the session with a new view of the most recently committed
    # GemStone state.
    #
    # These operations are performed whether or not the session was
    # previously in a transaction.  If the transaction mode is set to
    # :auto_begin, then a new transaction is started.  If the transaction
    # mode is set to :manual_begin, then a new transaction is not started.
    def abort_transaction
      return Gemstone.abort_transaction
    end

    # Returns true to indicate that the session is in a transaction, false
    # otherwise.
    def in_transaction?
      return Gemstone._in_transaction?
    end

    module_function :commit_transaction, :abort_transaction, :in_transaction?
  end
end
