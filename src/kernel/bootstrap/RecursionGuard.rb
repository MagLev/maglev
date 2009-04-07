module RecursionGuard

  def self.inspecting?(obj)
    Thread._recursion_guard_set.include?(obj)
  end

  def self.inspect(obj, &block)
    ts = Thread._recursion_guard_set
    added = ts._add_if_absent(obj)
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
    Thread._recursion_guard_set
  end
end
