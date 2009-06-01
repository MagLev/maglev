module Signal
  # We currently don't have any signals...
  SIGNALS = { }.freeze  # Hash of Signal name to number

  def self.trap(sig, command=nil, &block)
    # TODO: Signal.trap: MNI  Not Implemented
  end

  def self.list
    SIGNALS
  end
end
Signal._freeze_constants
