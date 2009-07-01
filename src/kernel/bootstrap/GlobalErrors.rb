# This file is loaded after Globals.rb.
# Definitions in this file need to be in separate file from
#   Globals.rb, to force all global definitions in Globals.rb
#  to be created/resolved before we attempt to use some of them here.

# See the exception hierarchy Pickaxe Fig 27.1 page 462.  Some of the
# exceptions listed in Pickaxe are directly mapped to a Smalltalk exception
# in Globals.rb.


class Interrupt           < SignalException; end

module Maglev

  class MaglevException < StandardError;  end
    
  class NotPersistableException < MaglevException;  end

  class OutsideOfTransactionException < MaglevException; end
    
  class CommitFailedException < MaglevException ; end

end

