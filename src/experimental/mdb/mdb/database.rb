# A Database holds a set of documents, views on those documents and manages
# indicies on the documents.  The Database class manages a persistent hash
# of the currently registered Database instances.
module Maglev
  def transaction(&block)
    Maglev.abort_transaction
    Maglev.persistent do
      block.call
    end
    Maglev.commit_transaction
  end
  module_function :transaction
end

module MDB
  # All MDB errors derive from MDBError
  class MDBError < RuntimeError; end

  class Database

    # holding on to a view class may be problematic, as each time we update
    # the view class (with a new view)
    def initialize
      @views     = nil
      @documents = IdentitySet.new
    end

    def run_view(view_name)
      @views.send(view_name.to_sym) unless @views.nil?
    end

end

