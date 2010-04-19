module VSD
  # A StatSeries holds many samples of the cache statistics for a
  # particular session.
  class StatSeries

    # Return a new series for the Shared Page Cache
    def self.spc_series
      new(0)
    end

    # Return a new series for the current VM statistics.
    def self.my_series
      new(Maglev::System.my_cache_slot)
    end

    # Initializes a new StatSeries.  No samples are taken.
    def initialize(session_id = Maglev::System.session_id)
      @session_id = session_id
      @samples = []
    end

    # Take a snapshot of the cache statistics for the session represented
    # by receiver.  Returns receiver.
    def sample
      @samples << SessionStats.new(@session_id)
      self
    end

    # Return the value for <tt>stat_name</tt> in the sample at index
    # +sample+ (by default, the last sample). Raises an exception if there
    # are no samples or +sample+ is too big.  Raises an argument error if
    # <tt>stat_name</tt> is not the name of one of the statistics.
    def [](stat_name, sample=-1)
      raise 'Not enough samples' if @samples.size == 0 || @samples.size < sample
      @samples[sample][stat_name]
    end

    # Return a new SessionStats object that represents the difference
    # between two samples.  By default, the difference is between the first
    # and last samples (@samples[-1] - @samples[0]).
    # Raises an exception if there are not at least two samples.
    def diff(index_a=0, index_b=-1)
      raise 'Not enough samples' unless @samples.size > 1
      @samples[index_b] - @samples[index_a]
    end

    # Return the number of samples currently held by receiver.
    def sample_count
      @samples.size
    end
  end
end
