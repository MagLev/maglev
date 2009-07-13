# Persistent Employee class from the persistence-api doc

Maglev::PERSISTENT_ROOT[:employees] = Array.new
Maglev.persistent do
  class Employee
    attr_reader :name, :salary
    def initialize(name, salary)
      @name = name
      @salary = salary
    end

    def to_s
      "Hi, I'm #{@name} and I make $#{@salary}."
    end
    def self.new(*args)
      instance = super
      instance.initialize(*args)
      Maglev::PERSISTENT_ROOT[:employees] << instance
      instance
    end
  end
end
3.times do |i|
  Employee.new("Person#{i}", (i * 5000) + 1)
end
Maglev.commit_transaction
