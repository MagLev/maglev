# From rubinius, but not all of common/dir.rb
class Dir
  include Enumerable

  def self.[](*patterns)
    if patterns.__size._equal?( 1)
      pat = patterns[0]
      pat = Maglev::Type.coerce_to(pat, String, :to_str)
      patterns = pat.split("\0")
    end
    files = []
    patterns.each do |pat|
      pat = Maglev::Type.coerce_to(pat, String, :to_str)
      Dir::Glob.glob(pat, 0, files)
    end
    files
  end

  def self.glob(pat, flags=0, &block)
    if pat._isArray
      patterns = pat
    else
      pat = Maglev::Type.coerce_to(pat, String, :to_str)
      return [] if pat.__size._equal?(0)
      patterns = pat.split("\0")
    end
    matches = []
    patterns.each { |apat|
      apat = Maglev::Type.coerce_to(apat, String, :to_str)
      Dir::Glob.glob( apat, flags, matches)
    }
    if block_given?
      matches.each(&block)
      return nil
    end
    return matches
  end


  def self.join_path(p1, p2, dirsep)
    "#{p1}#{dirsep ? '/' : ''}#{p2}"
  end
end
