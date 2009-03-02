# MagLev doesn't handle destructuring binds correctly ("subassignment" per
# page 99 of the Flanagan/Matz book).  There are two issues.
#
# Issue one:
#
# The following code compiles, but gets the wrong answer here,
# ["Ignore", "Ignore2"] rather than [1,1].  (You have to comment out the
# failing code below in order to run this one)
remotes = [
  [["Name1", 1, "Whatever"],  "Ignore"],
  [["Name2", 2, "Whatever2"], "Ignore2"],
]
remotes = remotes.map { |(name, version,_),_| version }
raise "Fail: #{remotes.inspect}" unless remotes == [1,2]

# Issue two:
#   The same code, but in a method defn, won't even compile
class SourceIndex
  def outdated
    #
    remotes = [
      [["Name1", 1, "Whatever"],  "Ignore"],
      [["Name2", 2, "Whatever2"], "Ignore2"],
    ]
    remotes = remotes.map { |(name, version,_),_| version }
    raise Fail unless remotes == [1,2]
    puts remotes.inspect
  end
end

SourceIndex.new.outdated
