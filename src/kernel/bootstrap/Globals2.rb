# This has to be loaded after Exception.rb is loaded.

# TODO: This is a lie.  Since primitives get saved in the stone, all vms
# running off of the stone that last loaded primitives will get this value.
RUBY_PLATFORM  = %w( not_used sparc_solaris linux-gnu PowerPC_AIX
                     darwin9.0 x86_64_Solaris Itanium_HP-UX
                    )[Exception._cpuOsKind - 1]

