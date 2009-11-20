class BadCode
  Version = 2
  attr_reader :id, :migrated
  def initialize(id)
    @id = id
    @migrated = false
  end

  def hello();        puts "hi" end

  def migrate_from(other)
    super
    @migrated = true
    raise "I don't want to migrate" if id == 0
  end
end

