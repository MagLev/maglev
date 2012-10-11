# https://github.com/MagLev/maglev/issues/143
#
# gsub has only specialized methods and thus cannot be aliased

$gsub_runs = []

class String
  def gsub_with_logging(*arg, &block)
    $gsub_runs << caller
    gsub_without_logging(*arg, &block)
  end
  alias_method :gsub_without_logging, :gsub
  alias_method :gsub, :gsub_with_logging
end

"hello".gsub("h", "b")
raise unless $gsub_runs.size == 1

"hello".gsub("h") { "b" }
raise unless $gsub_runs.size == 2

# Test passes if no exception is thrown
