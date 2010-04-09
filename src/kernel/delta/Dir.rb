class Dir
  include Enumerable

  # Overrides of Enumerable
  def entries
    check_closed
    @_st_entries
  end
end
