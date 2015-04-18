
Overview of the bootstrap sequence

  Stone is started with a copy of $GEMTONE/bin/extent0.dbf
  which is the initial Smalltalk server repository.

  topaz runs
    src/smalltalk/baseruby.gs
    src/smalltalk/loadfiletree.gs 

  baseruby.gs loads Monticello and a few other pieces of Smalltalk.
  The Monticello code allows reading of Smalltalk code stored
  in disk files in Cypress format .

  loadfiletree.gs  loads the Smalltalk code which is in the
  Cypress directory and file  format in src/packages/   .
  (In the early stages of Maglev 1.8 this code was in a .mcz file).
  This Smalltalk code includes parser support and Smalltalk implementations
  of "Ruby primitives" .

  topaz runs
    src/smalltalk/ruby/allprims.gs
  This loads all of the Ruby code that must be committed
  to the repository before normal ruby execution can happen.
  This includes all of the .rb files under src .
