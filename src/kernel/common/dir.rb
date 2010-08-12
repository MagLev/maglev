# From rubinius, but not all of common/dir.rb
class Dir
  include Enumerable

  def self.[](*patterns)
    files = []
    patterns.each do |pattern|
      pat = Type.coerce_to(pattern, String, :to_str)
      files.concat glob(pat, 0)
    end
    files
  end

  def self.glob(pat, flags = 0)
    pattern = Type.coerce_to(pat, String, :to_str)
    return [] if pattern.empty?
    Dir::Glob.glob pattern, flags
  end

  def self.join_path(p1, p2, dirsep)
    "#{p1}#{dirsep ? '/' : ''}#{p2}"
  end
end
