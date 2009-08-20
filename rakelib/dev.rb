# Ruby support for the rake tasks in dev.rake.  Support for managing the
# development environment.
# Returns true iff there is a process listening on the PARSETREE_PORT

# ######################################################################
#                           TOPAZ COMMAND STRINGS
# ######################################################################
# The following +tc_*+ methods generate topaz command strings based on the
# parameters passed to them.
class String
  # This makes it nice to indent here documents and strip
  # out the leading space from the here document.  A here
  # document using this uses '|' to mark the beginning of line.
  # E.g.,
  #
  #   str = <<END.margin
  #       | This is the string.  All spaces before the '|
  #       | and the '|' will be stripped from each line...
  #   END
  #
  # From section 2.3 of The Ruby Way, 2nd Ed.
  def margin
    arr = self.split("\n")
    arr.map! { |x| x.sub!(/\s*\|/,"") }
    str = arr.join("\n")
    self.replace(str)
  end
end

# Returns a topaz command string that runs the set of passing vm tests
# in src/test/vmunit.conf
def tc_run_vmunit
  <<-END.margin
    |omit resultcheck
    |run
    |RubyContext _runVmUnit
    |%
    |exit
  END
end

# Returns a topaz command string that will generate Ruby files that expose
# every smalltalk class.
def tc_gen_st_wrappers
  <<-END.margin
    |omit resultcheck
    |run
    |RubyContext createSmalltalkFFIWrappers
    |%
    |exit
  END
end

# Returns a topaz command string that will load the native parser
def tc_load_native_parser
  <<-END.margin
    |inp #{"src/kernel/parser/loadrp.inp"}
  END
end

# # Returns a topaz command string that loads ../latest.mcz and commits the DB.
# def tc_load_mcz
#   <<-END.margin
#     |output push loadmcz.out
#     |display resultcheck
#     |run
#     || fileRepo aName ver |
#     |fileRepo := MCDirectoryRepository new directory: (FileDirectory on: '../').
#     |aName := 'latest.mcz'.
#     |
#     |ver := fileRepo loadVersionFromFileNamed: aName .
#     |ver class == MCVersion ifFalse:[ aName error:'not found in repos' ].
#     |GsFile gciLogServer: ver printString .
#     |ver load .
#     |GsFile gciLogServer: 'load done'.
#     |^ true
#     |%
#     |commit
#     |exit
#   END
# end
