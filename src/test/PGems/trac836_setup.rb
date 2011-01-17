# Play with persistent autoloads
#
# Goal: Try to get autoloads to be persisted in one vm, and then triggered
# in the next.
#
# Steps to reproduce:
#
# 1. Run this file, which requires the foo library.  Module Foo defines an
#    autoload for the class Foo::Basic.
#
# 2. In a new vm, run the autoload client, that tries to create a
#    Foo::Basic object.  This triggers the autoload
#
Maglev.persistent do
  require 'foo'
end
Maglev.commit_transaction

