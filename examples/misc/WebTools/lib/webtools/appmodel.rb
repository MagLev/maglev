# This module emulates all of the API from the Smalltalk side of things
module WebTools
  class AppModel

    # Returns a hash of configuration parameters for the stone and the gem.
    # Each value is an array with the stone's value at index 0 and the
    # gem's value (if different than the stone's value) at index 1.
    #  { 'foo' => ['stone_value', 'gem_value_if_different'],
    #    'bar' => ['shared_value', ''] }
    #
    def version_report
      stone_rpt = stone_version_report
      gem_rpt = gem_version_report
      data = { }
      (stone_rpt.keys + gem_rpt.keys).each do |k|
        g = stone_rpt[k] == gem_rpt[k] ? '' : gem_rpt[k]
        data[k] = [stone_rpt[k], g]
      end
      data
    end

    def stone_version_report
      results = { }
      rpt = Maglev::System.stone_version_report
      rpt.keys.each { |k| results[k] = rpt.at(k) }
      results
    end

    def gem_version_report
      results = { }
      rpt = Maglev::System.gem_version_report
      rpt.keys.each { |k| results[k] = rpt.at(k) }
      results
    end
  end

end
