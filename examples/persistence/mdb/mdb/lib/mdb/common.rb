
# TODO: Need to figure out the txn model.  Should we assume we are not in a
# txn?  I.e., should we do abort or continue or nothing before the
# operation and then committing?

module MDB
  # All MDB errors are descendents of MDB::MDBError
  class MDBError < RuntimeError; end

  # All MDB exceptions (that are not errors) are descendents of
  # MDB::MDBException.
  class MDBException < Exception; end

  # The shared persistent counter to use for MDB.  This is used for
  # document ids by MDB::Database.
  SHARED_COUNTER = 128
end

module Maglev
  # Run block in a Maglev persistent block, and then commit.
#   def ptransaction
#     Maglev.persistent do
#       yield
#     end
#     Maglev.commit_transaction
#   end
#   module_function :ptransaction

  # Run block and then commit.
  def transaction
    # Should we abort or continue?
    Maglev.abort_transaction
    Maglev.persistent do
      yield
    end
    Maglev.commit_transaction
  end
  module_function :transaction
end
