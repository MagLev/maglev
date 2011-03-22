# Support to emulate the Smalltalk code in $MAGLEV_HOME/gemstone/examples/www

module Maglev
  class System
    class_primitive_nobridge 'stone_version_report', 'stoneVersionReport'
    class_primitive_nobridge 'gem_version_report',   'gemVersionReport'
  end
end

# The stoneVersionReport returns a StringKeyValueDictionary.
# Unfortunately, that is on a different branch of the Smalltalk class
# hierarchy than RubyHash, so we need to add a few methods to let us
# iterate over the dictionary.
StringKeyValueDictionary = __resolve_smalltalk_global(:StringKeyValueDictionary)
class StringKeyValueDictionary
  primitive_nobridge 'keys', 'keys'
  primitive_nobridge 'at', 'at:'
end

module WebTools
  # This class emulates all of the API from the Smalltalk side of things
  module API
    def stone_version_report
      results = { }
      rpt = Maglev::System.stone_version_report
      rpt.keys.each { |k| results[k] = rpt.at(k) }
      results
    end
    module_function :stone_version_report

    def gem_version_report
      results = { }
      rpt = Maglev::System.gem_version_report
      rpt.keys.each { |k| results[k] = rpt.at(k) }
      results
    end
    module_function :gem_version_report
  end
end
