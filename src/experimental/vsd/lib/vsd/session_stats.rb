require 'vsd/utils'

module VSD
  # Each session attached to a repository/shared page cache has its own
  # statistics.  An instance of this class manages one slot in the stats
  # table.
  class SessionStats

    # Create an index of statistic name to its index into the stats object
    NAME_TO_INDEX = Hash.new
    Maglev::System._cache_statistics_description.each_with_index do |name,i|
      NAME_TO_INDEX[name] = i
    end

    STAT_NAMES = NAME_TO_INDEX.keys.sort

    def self.my_index
      Maglev::System.my_cache_slot  # RxINC: adjust from smalltalk? -1 ?? do it in System
    end

    # return the statistics object for the local shared page cache.
    def self.spc_statistics
      SessionStats.new(0, "Shared Page Cache")
    end

    # Return the statistics for the current VM session
    def self.my_statistics
      SessionStats.new(Maglev::System.my_cache_slot)
    end

    def self.all_statistics
      stats = []
      (0..Maglev::System.session_count).each do |id|
        stats << SessionStats.new(id)
      end
      stats
    end

    attr_reader :name, :stats

    # Initializes a new SessionStats object.  The object will monitor
    # statistics in the session slot +session_id+ (See
    # Maglev::System.session_id).  The name used in reporting the stats.
    #
    # When an instance is created, the current set of statistics is read
    # and stored internally.  The statistics can be refreshed with the
    # #refresh method.
    def initialize(session_id = Maglev::System.session_id, a_name = nil, stats = nil)
      @session_id = session_id
      @stats = stats
      refresh if stats.nil?
      @name = a_name.nil? ? self['ProcessName'] : a_name  # after @stats is filled...
    end

    def to_s
      "#<SessionStats: #{name} pid #{self['ProcessId']} index #{@session_id}>"
    end

    # Refresh the statistics from shared memory.
    def refresh
      @stats = Maglev::System._cache_statistics(@session_id)
    end

    # Return the current value of the statistic named <tt>stat_name</tt>.
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

    # def x(other)
    #   raise ArgumentError, "Only takes another SessionStats" unless other.kind_of? SessionStats
    #   new_stats = []
    #   other_stats = other.stats
    #   @stats.each_with_index do |v,i|
    #     if v.kind_of? Numeric
    #       new_stats[i] = v - other_stats[i]
    #     else
    #       new_stats[i] = v
    #     end
    #     new(@session_id, "#{@name} - #{other.name}", other_stats)
    #   end
    # end

    # Given a block, a zero element and several enums, call the block with
    # the first element from all the enums, then call it with the second
    # elements from all enums etc.  E.g., to do the element-wise sum of
    # several arrays:
    #
    #    mapcar(0, [0,0,0], [1,2,3], [2,4,6]) {|x,y| x + y} # => [3,6,9]
    def mapcar(zero, *args)
      zargs = args[0].zip(*args[1..-1])

      # Technically, I guess I should do:
      #    zargs.map {|x| yield(zero, *x) }
      # or
      #    zargs.map {|x| yield(*x) }
      zargs.map {|x| x.inject(zero) {|s,i| yield(s,i) } }
    end
  end

end
