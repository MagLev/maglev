require 'vsd/utils'

module VSD

  # session stats object will save the current set of statistics in its
  # internal storage.  The class provides several methods for diffing,
  # comparing and printing trends on the series of samples.



  # A SessionStats object stores one set of VM Cache statistics.  The
  # statistics are read from the cache at object initialization.  The
  # statistics are accessed by name, e.g., 'ProcessName', 'StnLoopCount',
  # etc.  The full list of names is available in
  # <tt>SessionStats::STAT_NAMES</tt>.
  class SessionStats

    # Create an index of statistic name to its index into the stats object
    NAME_TO_INDEX = Hash.new
    Maglev::System._cache_statistics_description.each_with_index do |name,i|
      NAME_TO_INDEX[name] = i
    end

    STAT_NAMES = NAME_TO_INDEX.keys.sort

    # Return the index into the cache statistics for the current VM's
    # session.
    def self.my_index
      Maglev::System.my_cache_slot
    end

    # Return the statistics object for the shared page cache servicing the
    # current VM..
    def self.spc_statistics
      SessionStats.new(0, "Shared Page Cache")
    end

    # Return the statistics for the current VM session
    def self.my_statistics
      SessionStats.new(Maglev::System.my_cache_slot)
    end

    # Return an array of SessionStats objects, one for each currently
    # active sesssion.
    def self.all_statistics
      stats = []
      (0..Maglev::System.session_count).each do |id|
        stats << SessionStats.new(id)
      end
      stats
    end

    attr_reader :name, :stats

    # No statistics are read until +#sample+ is called.
    def initialize(session_id = Maglev::System.session_id, a_name = nil, stats = nil)
      @session_id = session_id
      @stats = stats || Maglev::System._cache_statistics(@session_id)
      @name = a_name || self['ProcessName'] # @stats must be initialized
    end

    def to_s
      "#<SessionStats: #{name} pid #{self['ProcessId']} index #{@session_id}>"
    end

    # Return the value for <tt>stat_name</tt>. Raises an argument error if
    # <tt>stat_name</tt> is not the name of one of the statistics.
    def [](stat_name)
      idx = NAME_TO_INDEX[stat_name]
      raise ArgumentError, "Unknown stat name #{stat_name}" if idx.nil?
      @stats[idx]
    end

    # Returns a new SessionStats that represents the difference between
    # self and other (self.stats[x] - other.stats[x]) for all numeric
    # stats.  The name reflects the operation.
    def -(other)
      raise ArgumentError, "Only takes another SessionStats" unless other.kind_of? SessionStats
      new_stats = Utils.mapcar(@stats, other.stats) {|x,y| x.kind_of?(Numeric) ? x - y : x}
      SessionStats.new(@session_id, "#{@name} - #{other.name}", new_stats)
    end
  end

end
