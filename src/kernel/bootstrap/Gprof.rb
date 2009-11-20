# Class Maglev::Gprof is identically Smalltalk ProfMonitorTree
#

module Maglev
  Gprof = Object._resolve_smalltalk_global(:ProfMonitorTree)
  class Gprof

    # returns a String which is the gprof-style profiling analysis of
    # statistical sampling during execution of block.
    # default sample interval is 100 microseconds .
    class_primitive '__monitor&', 'monitorIntervalNs:block:'

    def self.monitor(interval_ns = 100000, &block)
       self.__monitor(interval_ns) { block.call }   
    end
  end
end
