# Second version of the MigrationTest class.
class MigrationTest
  VERSION = 1

  attr_reader :id, :color
  def initialzie(id, color)
    @id = id
    @color = color
  end

  def migrate_from(other)
    super # Copies current instance variables
    @color = :unknown
  end
end

