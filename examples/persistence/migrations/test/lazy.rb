=begin

  Create an example of several schema changes, create data at each version,
  demonstrate lazy migration

  + Add a field
  + Remove a field
  + Rename a field
  + Reformat a field

  + Deal with sub-classes
  + Deal with references to the class in other objects (both by name and by
    oop).
  + How do you migrate attr_* methods? Or hand-written methods?
    If you rename @a to @b, do we change

=end

Maglev.persistent do
  class Point
    VERSION = "1.0.0"

    attr_reader :x, :y
    def initialize(x, y)
      @x = x
      @y = y
    end

    def 
    end
  end
end

Maglev::PERSISTENT_ROOT[]

