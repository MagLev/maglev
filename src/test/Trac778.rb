# Distilled from the rails/thor generator code
#
# During execution of the block passed to create_file, the block variable
# (from the home method) is nil.  After execution of the result.(context)
# call, the block variable is now a string.  It should still be nil.

class ERB
  def result(b=TOPLEVEL_BINDING)
    proc { eval("10", b) }.call
  end
end

class CreateFile
  attr_reader :data

  def initialize(data, config={})
    @data = data
  end
end

class Base
  def create_file(destination, *args, &block)
    x = CreateFile.new(block, { })
    x.data.call
  end

  def template(source, *args, &block)
    destination = "x"
    context = instance_eval('binding')

    create_file nil, nil, { } do
      erb = ERB.new

      puts "before context call: block.nil? #{block.nil?}"
      raise "Fail A" if block

      # At this point, block is nil
      erb.result(context)

      # At this point, block should still be nil, but isn't
      raise "Fail B" if block

      content = block.call(content) if block
      content
    end
  end
end

Base.new.template("Rakefile")
