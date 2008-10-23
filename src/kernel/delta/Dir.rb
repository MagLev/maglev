class Dir
  include Enumerable

  # Overrides of Enumerable
  def entries
    check_closed
    @entries
  end
end
