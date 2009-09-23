module MDB
  # All MDB errors are descendents of MDB::MDBError
  class MDBError < RuntimeError; end

  # All MDB exceptions (that are not errors) are descendents of
  # MDB::MDBException.
  class MDBException < Exception; end
end

module Maglev
  # Run block in a Maglev persistent block, and then commit.
  def transaction(&block)
    Maglev.abort_transaction
    Maglev.persistent do
      block.call
    end
    Maglev.commit_transaction
  end
  module_function :transaction
end
