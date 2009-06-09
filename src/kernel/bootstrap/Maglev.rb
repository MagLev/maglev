# This module defines a convenient constant in the global namespace so
# scripts can quickly tell if they are running MagLev:
#
#    if defined? Maglev
#      ...
#    end
#
module Maglev
  # The root for all Maglev exceptions
  class MaglevException < StandardError;  end

  class OutsideOfTransactionException < MaglevException; end

  # This exception is raised if MagLev is unable to commit the state of the
  # repository.  The details of why the commit failed are contained in the
  # exception.
  class CommitFailedException < MaglevException
    # TODO: De-smalltalk-ify description and API
    #
    # Returns a SymbolDictionary that contains an Association whose key is
    # #commitResult and whose value is one of the following Symbols:
    # #success, #failure, #retryFailure, #commitDisallowed, or #rcFailure .
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

    # TODO: Should we have something simple like this to handle the "Note"
    # at the end of the description for transaction_conflicts?
    #
    # Disconnect (ignore) all conflicts
    def disconnect_conflicts
      raise NotImplementedError
    end
  end

  # Attempts to update the persistent state of the Repository to include
  # changes made by this transaction.
  #
  # If the commit operation succeeds, then this method returns true, and
  # the current transaction's changes, if any, become a part of the
  # persistent Repository.  After the repository update, the session exits
  # the current transaction.  If the transaction mode is :auto_begin (the
  # MagLev default), then the session enters a new transaction.  If the
  # transaction mode is :manual_begin, then the session remains outside of
  # a transaction.
  #
  # If conflicts prevent the repository update, then this method raises a
  # CommitFailedException which contains details of why the commit failed.
  # Call the transaction_conflicts method to determine the nature of the
  # conflicts.  If the session is outside of a transaction, then this
  # method raises the error OutsideOfTransactionException
  #
  # This method also updates the session's view of GemStone.  If the commit
  # operation succeeds, then all objects in the session's view are
  # consistent with the current state of GemStone.  If the commit fails,
  # then this method retains all the changes that were made to objects
  # within the current transaction.  However, commits made by other
  # sessions are visible to the extent that changes in this transaction do
  # not conflict with them.
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

  module_function :commit_transaction, :abort_transaction
end
