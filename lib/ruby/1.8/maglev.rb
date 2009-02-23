# The MagLev module contains sub-modules that all expose the same API, but
# implement different semantics on whether code or data or both are
# saved to or read from the repository
#
# Other things to consider:
#
#   * Should we expose an API to get a persistent root?
#
#   * Does the $ namespace get blown away with a reload of prims?
#
#   * Some way to checkpoint the "data" (i.e., assume no code changes
#     matter, take a snapshot of current data, and then at the end of the
#     checkpoint, abort the transaction.
#
#   * Some way to unset a global (remove it from the namespace, not just
#   * set to nil).
module MagLev  # TODO: "MagLev" or "Maglev"?

  # This module exposes three methods to commit, abort and begin a
  # transaction, but also manages the ruby context to ensure that both code
  # and data are always saved/restored.
  module CodeAndData
    def begin_txn
      Gemstone.beginTransaction
      RubyContext.load_context
    end

    def abort_txn
      Gemstone.abortTransaction
    end

    def commit_txn
      begin
        RubyContext.save_context
        Gemstone.commitTransaction
      rescue
        # Nothing to do?  Can't reverse RubyContext.save_context...
      end
    end

    # Aborts the current transaction, then loads code and data changes from
    # the repository
    def freshen
      Gemstone.abortTransaction
      RubyContext.load_context
      true
    end

    module_function :begin_txn, :abort_txn, :commit_txn, :freshen
  end

  # This module exposes three methods to commit, abort and begin a
  # transaction, but only commits
  # and data are always saved/restored.
  module DataOnly
    def begin_txn
      Gemstone.beginTransaction
    end

    def abort_txn
      Gemstone.abortTransaction
    end

    def commit_txn
      begin
        Gemstone.commitTransaction
      rescue
        # Nothing to do?  Can't reverse RubyContext.save_context...
      end
    end

    # Aborts the current transaction, then loads code and data changes from
    # the repository
    def freshen
      Gemstone.abortTransaction
      true
    end

    module_function :begin_txn, :abort_txn, :commit_txn, :freshen
  end
end
