# This class represents the name and location of each of the postal codes
# (zip codes) in the US.  It conforms to the api expected for Tree2D.
class PostalCode

  # Parse a file of postal code information
  def self.parse_file(file_name)
    nodes = Array.new
    File.open(file_name) do |f|
      f.each_line do |line|
        nodes << parse_line(line)
      end
    end
    nodes
  end

  # Initialize a PostalCode from its entry in the data file
  def self.parse_line(line)
    fields = line.chomp.split("\t")
    new(*fields)
  end

  def initialize(cc, pc, pn, an1, ac1, an2, ac2, an3, lat, lon, acc=nil)
    raise ArgumentError, "Not a US zip code #{cc}" unless cc == "US"
    @zip = pc
    @name = pn
    @admin_name_1 = an1
    @admin_code_1 = ac1
    @admin_name_2 = an2
    @admin_code_2 = ac2
    @admin_name_3 = an3
    @lat = lat.to_f
    @lon = lon.to_f
  end

  def to_s
    "#{@name} [#{@admin_name_1} #{@admin_name_2} #{@admin_name_3}] #{@zip}  #{@lat} #{@lon}"
  end

  # API needed by Tree2D
  def x; @lat end
  def y; @lon end
  def dist_sq(other)
    # ok...this is euclidean distance, not spherical or anything
    # realistic...
    dx = @lat - other.x
    dy = @lon - other.y
    (dx * dx) + (dy * dy)
  end
end
