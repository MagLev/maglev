# Author:: Nathaniel Talbott.
# Copyright:: Copyright (c) 2000-2003 Nathaniel Talbott. All rights reserved.
# License:: Ruby license.

require 'test/unit/collector'

module Test
  module Unit
    module Collector
      class ObjectSpace
        include Collector

        NAME = 'collected from subclasses of TestSuite'

        def initialize()
          super()
        end

        def collect(name=NAME)
          suite = TestSuite.new(name)
          sub_suites = []
          TestCase::DECENDANT_CLASSES.each do |klass|
            add_suite(sub_suites, klass.suite)
          end
          sort(sub_suites).each{|s| suite << s}
          suite
        end
      end
    end
  end
end
