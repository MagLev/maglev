RubyCompiler = __resolve_smalltalk_global(:RubyCompiler)
class RubyCompiler
  primitive_nobridge '_compile', 'compileString:loadName:'

  # Compile and evaluate ("run") ruby source code from a string.
  # file_name is used as the file name associated with the source (__FILE__
  # will be set to this in the context of the code in ruby_source_string).
  #
  # == Example
  #
  # This code:
  #
  #     require 'maglev/ruby_compiler'
  #     source_string = <<'EOS'
  #     class Who
  #       def hello
  #         puts "Hello, I was compiled from a ruby string."
  #         puts "(but I *think* I was compiled from #{__FILE__})"
  #       end
  #     end
  #     EOS
  #
  #     RubyCompiler.new.compile(source_string, 'Some Bogus File Name (or other)')
  #     Who.new.hello
  #
  # produces:
  #
  #     $ maglev-ruby foo.rb
  #     Hello, I was compiled from a ruby string.
  #     (but I *think* I was compiled from Some Bogus File Name (or other))
  #
  def compile(ruby_source_string, file_name="<a String>")
    _compile(ruby_source_string.freeze, file_name)
  end

end
