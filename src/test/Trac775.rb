# Distilled from Rails Generators / Thor
#
# Maglev handling of is_a?(Proc) broken in an || expression.
#

class AppGenerator
  def template(source, *args, &block)
    config = args.last.is_a?(Hash) ? args.pop : {}
    destination = args.first || source

    create_file(destination, nil, config) do
      10
    end
  end

  def create_file(destination, *args, &block)
    config = args.last.is_a?(Hash) ? args.pop : {}
    data = args.first

    # MagLev thinks block is_a?(Proc)
    puts "A: block.is_a?(Proc): #{block.is_a?(Proc)}"
    puts "block.is_a?(Proc): #{block.is_a?(Proc)}"
    raise "FAIL A" unless block.is_a?(Proc)

    # If we call CreateFile.new w/o the || expression, things work:
    x = CreateFile.new(self, destination, block, config)
    puts "B: x.data.is_a?(Proc): #{x.data.is_a?(Proc)}"
    raise "FAIL B" unless x.data.is_a?(Proc)

    # But If we call CreateFile.new with the || expression, things break:
    x = CreateFile.new(self, destination, block || data.to_s, config)
    puts "C: x.data.is_a?(Proc): #{x.data.is_a?(Proc)}"
    raise "FAIL C" unless x.data.is_a?(Proc)
  end

  class CreateFile
    attr_reader :data
    def initialize(base, destination, data, config={})
      @data = data
    end
  end
end

class AppBuilder
  private
  %w(template).each do |method|
    class_eval <<-RUBY, __FILE__, __LINE__ + 1
      def #{method}(*args, &block)
        @generator.send(:#{method}, *args, &block)
      end
    RUBY
  end

  def initialize(generator)
    @generator = generator
  end

  def template(*args, &block)
    @generator.send(:template, *args, &block)
  end

  def gemfile
    template "Gemfile"
  end
end

b = AppBuilder.new(AppGenerator.new)
args = []
b.send(:gemfile, *args)
