require 'expect'

class TopazError < RuntimeError
  attr_accessor :exit_status, :output

  def to_s
    # Get the child's exit code
    puts @exit_status >> 8
    puts @output
  end

  def initialize(exit_status, output)
    @exit_status = exit_status
    @output = output
  end
end

class String
  def execute_on_topaz_stream(topaz_stream)
    topaz_stream.puts(self)
  end
end

class Array
  def execute_on_topaz_stream(topaz_stream)
    join("\n").execute_on_topaz_stream(topaz_stream)
  end
end

class Topaz
  attr_accessor :output

  def initialize(stone, topaz_command="#{ENV['GEMSTONE']}/bin/topaz -l -T 200000")
    @stone = stone
    @output = []
    @topaz_command = "#{topaz_command} 2>&1"
  end

  def commands(topaz_commands_array)
    fail "We expect the stone #{@stone.name} to be running if doing topaz commands. (Is this overly restrictive?)" if !@stone.running?
    IO.popen(@topaz_command, "w+") do |io|
      consume_until_prompt(io)
      topaz_commands_array.each do | command |
        command.execute_on_topaz_stream(io)
        if command != "exit" then
          consume_until_prompt(io)
        end
      end
    end
    if $?.exitstatus > 0
      raise TopazError.new($?, @output)
    end
    return @output
  end

  def dump_as_script(*topaz_commands)
    topaz_commands.each do | command |
      command.execute_on_topaz_stream(STDOUT)
    end
    self
  end

  private

  def consume_until_prompt(io)
    if result = io.expect(/(^.*> $)/)
      # remove prompt from output
      command_output = result[0].gsub(result[1], "")
      @output << command_output if not command_output.empty?
    end
  end
end
