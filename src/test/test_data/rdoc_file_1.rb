# This blows up shortly after the "+=" is parsed
class RDoc
  # Parse a file
  def parse_files(options)
    @stats.num_files += 1
  end
end

