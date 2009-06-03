== Proposed New MagLev Persistence API

=== Overview

This document is a brief overview of the MagLev persistence model and API
intended for MagLev Alpha Developers.  Many details left out of this
document are available in the following places:

* The Persistence MSpecs (in <tt>src/test/persistence/</tt>) specify the detailed
  semantics of the methods.  Some of the comments outline use cases and
  provide additional insight.  Note: these mspecs are not yet runnable.
* The persistence API file (currently <tt>src/test/persistence/persistence.rb</tt>).
* The GemStone/S 64 Bit Programming Guide: provides a full discussion of
  the GemStone persistence model (including transactions, locking, etc.),
  http://seaside.gemstone.com/docs/GS64-ProgGuide-2.2.pdf.

==== Synopsis

Out of the box, MagLev will accommodate two persistence models:

1. If you take no explicit action in your code, then you get no
   persistence (MRI compatibility).
2. To take advantage of the MagLev VM's distributed, shared,
   cached, native object persistence, requires explicit action, by adding
   directives to your code.

In the no persistence model, nothing is saved to the repository, so MagLev
acts much like the standard MRI version of Ruby.

In the explicit persistence model:

* A Class is made persistable by doing: <tt>MyClass.maglev_persist!</tt>
* An Object is persisted by:
    * Ensuring that its class and all classes and mixed in modules along
      its super class chain are persistable.
    * Attaching the object to a persistent root (e.g., <tt>USER\_GLOBALS</tt>)
    * Calling <tt>Maglev.commit_transaction</tt>

Other persistence mechanisms

  * ORM: Use the MySQL driver and your favorite ORM
  * PStore: Marshal to file system with Ruby PStore
  * OStore: Marshal to shared, cached MagLev persistent store
  * MStore: Native MagLev persistence behind a PStore interface

If none of these models is appropriate for your application, you may roll
your own using the full GemStone/S API via the Smalltalk FFI wrapper
classes.

This document is a work in progress and GemStone is looking for feedback on
how to improve the model.

==== Guiding Principles

The two principles that guided the persistence model are:

1. Unmodified MRI code, when run on MagLev, should run just like in MRI.
2. No unmodified code should be able to change the "structure of the DB"
   (add methods, add instance variables, modify constants, etc.).

Some of the other goals we had are:

* For the first round of the API, provide the minimal set of persistence
  primitives necessary to get things done.  GemStone is open to adding
  convenience methods in subsequent versions of the API.
* No magic: the programmer must explicitly request MagLev persistence,
  and by doing so, understands and deals with the effects of that decision.
* Keep the basic persistence model clean and simple, but still usable and
  powerful enough for many scenarios
* Provide full access to the entire GemStone persistence API for those who
  need it.
* The ruby code will provide enhancements over the analogous smalltalk API
  (e.g., <tt>commit_transaction</tt> throws an exception instead of returning
  false).

=== MRI Compatibility: Just like MRI

When a new MagLev VM starts up, it initializes its set of classes, methods
and objects from the GemStone Object DB managed by the stone.  The default
image shipped with MagLev has all the standard classes, modules,
objects and variables that one finds in a fresh MRI instance.

All new methods, instances and constants defined by code loaded into the
fresh VM are local to that VM, and will be lost when that VM exits.  No
other VM that attaches to the same stone will see modifications made by a
VM running in default mode.  File loading, etc. should be just like MRI.

=== Explicit MagLev Persistence

To take advantage of the transactional, shared, distributed, cached, native
MagLev persistence, takes explicit action by the MagLev programmer.  To
persist an object, its class must be persistent.

By default, MagLev runs in "auto-transaction" mode, which means that each
VM is always in a transaction.  If you stay in MRI world, there are no
visible consequences of this.  If you are in the MagLev world, then you
must explicitly commit the current transaction to save data.  The VM will
automatically start a new transaction after a commit or abort.

When you step into the Explicit MagLev Persistence Model, you've entered an
image based view of your program (the primary view of the code lives in the
image, not your files). Since, in the default MRI world, nothing is
committed to the database, the state of the VM is reflected closely by the
state of your <tt>.rb</tt> files.  But, as you begin to commit items into
the repository (and other VMs may also commit to the repository), the state
of the code may drift away from the state of the <tt>.rb</tt> files.  This
is not a big problem, just something to keep in mind.

* MRI is file world
* MagLev is image world

==== Persisting Classes

===== <tt>Class#maglev_persist!</tt>

<tt>Class#maglev_persist!</tt> makes a class persistable.  It marks the
class as persistable, so that instances of the class may be persisted.
Additionally, it stages for commit, the class, and all of its constants,
(class) instance variables, class variables, methods and class
methods. Finally, the constant naming the class, if appropriate, is
persisted within the appropriate namespace, if that namespace is persistent

A class may be persisted by issuing the following command:

    MyClass.maglev_persist!    # stage the class for persistence
    Maglev.commit_transaction  # commit the repository

In order to successfully persist a class, all of the classes in its super
class chain (including mixed in modules) must also be persisted.

The next time a VM starts up and connects to the stone, it will see the
state of the class at the time <tt>maglev_persist!</tt> was invoked and
committed.  Any VM already connected to the stone will see the new class
(or modifications) the next time it does a <tt>commit_transaction</tt> or
an <tt>abort_transaction</tt>.

All of the core classes (<tt>Object</tt>, <tt>Array</tt>, etc.) are already
marked persistable.

Note: persisting a class does not imply that any instances are
automatically persisted.  To persist an instance of a persisted class, the
instance must be explicitly attached to a persistent root by user code, and
then committed to the repository.

===== Re-opening a persisted class

Once a class has been persisted, it may subsequently be re-opened.  Only
those occurrences of a re-opening that are done within the scope of a
<tt>Module#maglev_reopen!</tt> will have their modifications persisted.

A short example:

    # Step 1
    #
    # First opening of the class
    class C
      A_CONST=1
      def initialize
        ...
      end
    end

    # Make the class persistable, and stage commits for A_CONST and
    # initialize.
    C.maglev_persist!

    ...

    # Step 2
    #
    # This re-opening of the class is not within the scope of a call to
    # C.maglev_reopen!, so none of these changes will be staged for
    # persistence.  The current VM will be the only VM to see
    # A_NON_PERSISTENT_CONST and a_non_persistent_method.
    class C
      A_NON_PERSISTENT_CONST = 42
      def a_non_persistent_method
      end

      def an_ambiguous_method
      end

    end

    ...

    # Step 3
    #
    # This re-opening of the class *is* within the scope of a call to
    # C.maglev_reopen!, so all of these changes will be staged for
    # persistence.  This will stage A_SECOND_PERSISTENT_CONST,
    # a_persistent_method, and an_ambiguous_method persistent for
    # persistence, but will NOT stage the other items from Step 2 for
    # persistence (i.e., A_NON_PERSISTENT_CONST and a_non_persistent_method
    # are still local to the VM and non-persistent; an_ambiguous_method
    # becomes persistent, with the definition from step 3, due to the
    # maglev_reopen! call).

    C.maglev_reopen do
      class C
        A_SECOND_PERSISTENT_CONST = 53

        def a_persistent_method
        end

        def an_ambiguous_method
        end
      end
    end
    Maglev.commit_transaction

==== Persisting "Data", or Objects

To persist an object, its class must first be marked
<tt>maglev_persist!</tt>, then, just follow the normal rules of MagLev
persistence:

* Attach the object to a persistent root
* Call <tt>Maglev.commit_transaction</tt>

===== Persistent Root

MagLev provides the <tt>Maglev::USER_GLOBALS</tt> Hash as the well-known
root for persistent objects.  An object does not need to connect directly
to <tt>Maglev::USER_GLOBALS</tt>, but just needs to be reachable from a
persistent root.

===== Use Case: Persist a core object

Since all of the core Ruby classes, including <tt>Object</tt>,
<tt>String</tt> etc. are already persistable, you can persist instances of
those classes immediately:

    # Persist an instance of a core object
    Maglev::USER_GLOBALS[:foo] = "Hi, I'm persistent"

    # Make a non-persistent instance
    $normal = "Hi, I'm just a regular, non-persistent string"

    # Commit to make permanent
    Maglev.commit_transaction

    # At this point, Maglev::USER_GLOBALS[:foo] is saved in the repository
    # and available to all VMs. $normal is, available only in this VM,
    # since it is not attached to a persistent root (global variables are
    # not persistent).

===== Use Case: Calling <tt>Maglev.abort_transaction</tt>

A <tt>Maglev.abort_transaction</tt> will set the value of all persistent
objects, including the state of <tt>Maglev::USER_GLOBALS</tt> to the
current state of the repository.  I.e., it will erase local changes to
persistent values, but leave local changes to local values alone.

    # Assume Maglev::USER_GLOBALS[:maybe] is nil:
    Maglev::USER_GLOBALS[:maybe]  # => nil

    # Stage an object for persistence
    Maglev::USER_GLOBALS[:maybe] = "I want to be persistent...but..."

    # create a local variable that will be unaffected by the
    # abort_transaction:
    $clueless = "Yup"

    Maglev.abort_transaction

    # At this point, the state of Maglev::USER_GLOBALS is reset to the
    # default value but local variables are unaffected

    Maglev::USER_GLOBALS[:maybe] # => nil
    $clueless # => "Yup"

===== Use Case: Persist a user defined object

To persist an object from a user defined class, you must first persist the
class, and then commit the object:

    # Define a class
    class Foo
       # stuff
    end

    # Stage the class for persistence
    Foo.maglev_persist!

    # Create a space in USER_GLOBALS to hold persistent Foo objects (here
    # we use the class as a key for finding instances of that class)
    Maglev.USER_GLOBALS[Foo] = Array.new

    # Connect a Foo instance to a persistent root
    Maglev.USER_GLOBALS[Foo] << Foo.new

    Maglev.commit_transaction  # commit class Foo and the instance f.

At this point, all VMs connected to the stone will see the new class
<tt>Foo</tt>, the new array in <tt>USER_GLOBALS[Foo]</tt> and the single
<tt>Foo</tt> object, contained in <tt>USER_GLOBALS[Foo]</tt> array.  All
new VMs that connect to the stone will also see these objects.

=== Other Persistence Options

This section outlines some persistence options that do not use the native
MagLev Object Persistence facility.

==== RDBMS + ORM

MagLev comes with the MySQL pure ruby driver.  You can use this to connect
to the MySQL DB and use any pure ruby ORM of your choosing (ActiveRecord
etc.).

The MagLev MySQL driver is shipped in the
<tt>lib/ruby/site_ruby/1.8/ruby-mysql</tt> directory, along with some
examples in <tt>examples/mysql</tt>.

==== PStore / Marshal

The Ruby standard PStore mechanism can store ruby objects to the file
system using the Ruby Marshaling facility.  PStore is shipped as part of
MagLev's standard ruby library.

==== OStore / Marshal

TODO: Need to re-write GStore to use Marshal.

OStore is a sample application that is the same as the Ruby PStore, but
stores the data in the MagLev DB rather than the file system.  It is a
half-way solution between MagLev native persistence and Ruby Marshaling.
OStore allows the persisted objects to share the distributed, cached nature
of GemStone native persistence, but bypass the need to save classes (OStore
uses Ruby Marshal to turn objects into strings).  OStore still suffers the
cost of marshal/unmarshal that is common to most ORMs.

One possible use for this model is to store HTTPSession data for a web
application, making it available for all VMs connected to the stone.

OStore is shipped as an example application in the
<tt>examples/persistence/ostore</tt> directory.

==== MStore??

Proposed: MStore is the PStore API using MagLev native persistence under
the covers, rather than marshalling to strings.  It is similar to the
current version of GStore.

MStore is shipped as an example application in the
<tt>examples/persistence/mstore</tt> directory.

==== Roll Your Own Persistence Model

If none of the preceding options meets your requirements, MagLev provides
access to the full GemStone/S API (transactions, locking, instance
migrations, etc.).  These APIs have proven to be robust and sufficient
enough for real world applications to run hundreds of VMs, thousands of
transactions per second and manage terabytes of data.

== Issues
* Eigenclass of a class (automatic when the class is marked?)
* Eigenclass of a non-class is automatically persisted when the object is
  persisted.
* GemStone decided not to include the ability to un-persist a class.  The
  underlying VM allows for classes to be made non-persistent, but until
  there is a good use case, we have not exposed this to the Ruby world due
  to the complexity of disentangling the class and its instances from the
  persisted object graph.
* Need a design document as well.  Some fodder for the design document:
  * Dale's idea: Calling <tt>MyClass.maglev_persist!</tt> should also write a
    copy of the new "file" to a well known directory so that the programmer
    can have a reference copy of what is in the image.  The VM will
    maintain the integrity of this shadow copy, since Ruby developers won't
    be developing in a squeak IDE.
  * To mark a class as persistable, move the ST method
    <tt>_makeInstancesPersistent</tt> up to behavior, that way we can mark a class'
    metaclass as persistent or not.  Then when in ruby you do
    <tt>MyClass.maglev_persist!</tt>, you can mark both the metaclass and the
    class as <tt>_makeInstancesPersistent</tt>.
  * We'll need to do some sort of shadow ruby name space, since that is
    where the names of the classes live.  If a class is persisted, then
    we'll need to ensure that it (and its inheritance chain) is also put in
    the persistent version of the ruby namespace.
  * Persisting a ruby class that has a (constant) reference to a
    non-persistable class or object will result in an exception.
* Other extensions:
  * There may be an opportunity to provide a .rbc form that does some of
    the compilation up front.
  * $LOADED_FEATURES is per VM and initialized to blank.
  * STDOUT $stdout
  * .rb files as migrations
  * print warnings on re-open of a persistent class if you did not do a
    maglev_reopen!?
  * Part of going to persistence world is you'll probably have to re-factor
    your code into init/run-once stuff and the "main loop".
  * Gemstone may have to partition popular ruby apps like IRB, gems etc.
* Does the effect of maglev_persist! have to be committed before you can
  commit an instance of the class, or can you do it in one shot. I.e., is
  this legal:

       # first mention of Foo
       class Foo
       end

       Foo.maglev_persist!

       # create an instance before Foo's persistence is committed
       # and attach to persistent root
       USER_GLOBALS[:foo] = Foo.new

       Maglev.commit_transaction   # will this fail or succeed? or is it
                                   # non deterministic?
* How will re-reading files work?  E.g., suppose you have:
    class MyClass
      Foo = 0
    end
    MyClass.maglev_persist!

  And you're in development, so you want to edit it and reload.  The API
  says you can call maglev_persist! only once.  Should we allow multiple
  calls in development mode?  Should we add a switch? Or should developers
  start w/o persisting, and then add it later?...
