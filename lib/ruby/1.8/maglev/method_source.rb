require 'maglev/ruby_compiler'
# Methods to get access to method source code.

class Module
  # Return the source code for a method.
  #
  # @param [String,Symbol] method_name  The name of the method
  # @param [Boolean] instance_method If true, look for an instance method
  #         named +method_name+, otherwise look in receiver's singleton
  #         class for the method.
  # @return [Array] Array of [String, String, Fixnum] method source, file name and line
  #
  def method_source(method_name, instance_method=true)
    gsnmeth = __gs_method(method_name, instance_method)
    src = gsnmeth.__source_string
    file,line = gsnmeth.__source_location
    [src,file,line]
  end

  def set_method_source(method_name, source, write_file = true)
    if self.ancestors.collect(&:to_s).include? "StClass"
      if source =~ /^\s+def\s+self\./m
        compile_source = source.sub(/def\s+self./, "def ")
      end
    end
    compile_source ||= source

    begin
      src, file, line = method_source(method_name, true)
    rescue Exception
      src, file, line = method_source(method_name, false)
    end

    if write_file
      unless File.writable?(file) && File.writable?(File.dirname(file))
        raise ArgumentError, "cannot write to method source and source directory"
      end
    end

    unless src
      raise ArgumentError, "not an ordinary method"
    end

    # Compile the new method
    line = line - 1
    self.class_eval(compile_source, file, line)

    if write_file
      # Write a new file with updated contents
      original_contents = File.readlines(file)
      copy = File.open("#{file}.tmp", 'w+') do |f|
        f.write(original_contents[0...line].join)
        f.write(original_contents[line..-1].join.sub(src, source))
      end

      # Rename to original file
      File.rename("#{file}.tmp", file)
    end

    [source, file, line + 1]
  end
end
