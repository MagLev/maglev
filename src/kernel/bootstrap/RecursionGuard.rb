module RecursionGuard

  def self.guarding?(obj)
    Thread.__recursion_guard_set.include?(obj)
  end

  def self.guard(obj, &block)
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(obj)
    if added
      begin
        yield
      ensure
        ts.remove(obj)
      end
    else
      yield
    end
  end

  def self.stack
    Thread.__recursion_guard_set
  end
end
