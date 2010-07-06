require 'tree2d'

# This class represents the name and location of each of the postal codes
# (zip codes) in the US.  It conforms to the api expected for Tree2D.
#
# It can read data in the format provided by
# http://www.geonames.org
#
class PostalCode < Collections::Point2D
  attr_reader :name, :zip

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
    super(lon.to_f, lat.to_f)
  end

  def lat
    y
  end

  def lon
    x
  end

  def state
    @admin_name_1
  end
  def county
    @admin_name_2
  end
  def to_s
    "#{@name} [#{@admin_name_1} #{@admin_name_2} #{@admin_name_3}] #{@zip}  #{lat} #{lon}"
  end

  RADIAN = Math::PI / 180.0
  MILES_PER_RADIAN = 3958.75

  # Approximate angular distance in radians between reciever and
  # other. Assumes both points are in same coordinate system.
  def spherical_distance(other)
    x_rad = lon * RADIAN
    y_rad = lat * RADIAN

    dx_rad = (x_rad - (other.x * RADIAN))
    dy_rad = (y_rad - (other.y * RADIAN))

    sin_dx = Math.sin(dx_rad / 2.0)
    sin_dy = Math.sin(dy_rad / 2.0)

    sin_sq_dx = (sin_dx * sin_dx)
    sin_sq_dy = (sin_dy * sin_dy)

    prod_cos = Math.cos(y_rad) * Math.cos(other.y * RADIAN)

    2 * Math.asin(Math.sqrt(sin_sq_dy + prod_cos * sin_sq_dx))
  end

  # Return the great circle miles (on Earth) between reciever and other.
  def spherical_miles(other)
    MILES_PER_RADIAN * spherical_distance(other)
  end
end
