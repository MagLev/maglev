require 'maglev/objectlog'

class Thread
  # Saves the Thread to the ObjectLog.
  #
  # @param [String] message the message under which to save the entry,
  #   defaults to inspect and timestamp
  # @param [Boolean] force_commit whether or not to force a commit.
  #   Defaults to nil, which means it'll commit if the session is clean or raise
  #   an error. Pass true to forcibly abort and commit only the log entry, pass
  #   false to only commit if the session is clean.
  # @return [DebuggerLogEntry] the saved entry
  # @raise [RuntimeError] raised if the session is dirty but no force_commit
  #   option has been passed
  def save(message = nil, force_commit = nil)
    if Maglev.needs_commit and force_commit.nil?
      raise RuntimeError, "Saving exception to ObjectLog, discarding transaction"
    end
    message ||= "#{inspect}-#{DateTime.now}"
    Maglev.abort_transaction if force_commit || !Maglev.needs_commit
    DebuggerLogEntry.create_continuation_labeled(message)
    Maglev.commit_transaction if force_commit || !Maglev.needs_commit
    self
  end

  # Ready a thread recovered from the ObjectLog for resumption
  def load
  end

  # Remove Thread from ObjectLog
  def delete
    if Maglev::System.needs_commit
      raise Exception, "Abort would loose data. Commit your data and try again"
    end
    Maglev.abort_transaction
    ObjectLogEntry.object_log.delete(@log_entry)
    Maglev.commit_transaction
  end

  def stack
    self.__stack_depth.times.collect do |idx|
      Frame.new(self.__method_at(idx + 1), idx + 1, self)
    end
  end

  def step(symbol)
    case symbol
    when :into
      self.__step_over_in_frame(0)
    when :over
      self.__step_over_in_frame(1)
    when :through
      raise NotImplementedError, "not implemented yet"
    when Fixnum
      self.__step_over_in_frame(arg)
    end
  end

  class Frame
    def initialize(gsmethod, st_idx, thread)
      @method = ruby_method(gsmethod)
      @index = st_idx
      @thread = thread
    end

    def restart
      @thread.__trim_stack_to_level(@index)
    end

    def pop
      @thread.__trim_stack_to_level(@index + 1)
    end

    def step(*args)
      raise StandardError, "can only step top of stack" unless @index == 1
      @thread.step(*args)
    end

    def inspect
      "#<Frame #{@index}: #{@method.inspect} >> #{thread.inspect}>"
    end

    private
    def ruby_method(gsmethod)
      label = gsmethod.__name
      cls = gsmethod.__in_class
      if label and cls.instance_methods.include?(label.to_s)
        # We are looking at an actual Ruby method
        return cls.method(label.to_sym)
      else
        Method.__basic_new.tap do |m|
          m.__method_env_selPrefix(gsmethod, 1, label)
          m.__obj = cls
          m.__bridge = RubyBridge.exec_meth_bridge_to(gsmethod)
          m.instance_variable_set("@_st_obj", cls)
          m.instance_variable_set("@_st_gsmeth", gsmethod)
        end
      end
    end
  end

  # The list of saved threads in the ObjectLog
  def self.saved_list
  end
end

RubyNameSpace = __resolve_smalltalk_global(:RubyNameSpace)
class RubyNameSpace
  primitive 'parent', 'parent'
  primitive 'my_class', 'myClass'
  primitive 'keys', 'keys'
  primitive '[]', 'at:'
end

class Module
  primitive '__transient_namespace', 'transientNameSpace:'
  primitive 'singleton_class?', 'isRubySingletonClass'

  # The namespace (lexical scope) in which the Module was defined
  def namespace
    if ts = __transient_namespace(1)
      return ts.parent
    end
  end

  def singleton_instance
    raise TypeError, "not a singleton class" unless self.singleton_class?
    raise NotImplementedError, "not implemented yet"
  end
end

class GsNMethod
  primitive '__is_method_for_block', 'isMethodForBlock'
  primitive '__source_string_for_block', '_sourceStringForBlock'
  primitive '__source_offset_first_send_of', '_sourceOffsetOfFirstSendOf:'
end

class Method
  primitive '__obj=', 'object:'
end

class UnboundMethod
  class_primitive '__basic_new', '_basicNew'
  primitive '__method_env_selPrefix', 'method:env:selPrefix:'
  primitive '__bridge=', 'bridge:'

  attr_reader :_st_gsmeth

  RubyBridge = __resolve_smalltalk_global(:RubyBridge)
  class RubyBridge
    class_primitive 'exec_meth_bridge_to', 'execMethBridgeTo:'
  end

  def in_class
    @_st_gsmeth.__in_class
  end

  def file
    (@_st_gsmeth.__source_location || [])[0]
  end

  def line
    (@_st_gsmeth.__source_location || [])[1]
  end

  def source
    if @_st_gsmeth.__is_method_for_block
      @_st_gsmeth.__source_string_for_block
    else
      @_st_gsmeth.__source_string
    end
  end

  def file=(path)
    raise TypeError, "cannot modifiy a block method" if is_method_for_block?
    @_st_gsmeth.__in_class.class_eval(source, path, line)
  end

  def line=(num)
    raise TypeError, "cannot modifiy a block method" if is_method_for_block?
    @_st_gsmeth.__in_class.class_eval(source, file, num)
  end

  def source=(str)
    raise TypeError, "cannot modifiy a block method" if is_method_for_block?
    self.original_source = source
    @_st_gsmeth.__in_class.class_eval(str, path, line)
  end

  def file_out
    if !is_def_method? || is_method_for_block?
      raise StandardError, "not an ordinary method def"
    end
    
    unless File.writable?(file) && File.writable?(File.dirname(file))
      raise StandardError, "cannot write to method source and source directory"
    end
    
    # Write a new file with updated contents
    original_contents = File.readlines(file)
    copy = File.open("#{file}.tmp", 'w+') do |f|
      f.write(original_contents[0...line].join)
      f.write(original_contents[line..-1].join.sub(original_source, source))
    end

    # Rename to original file
    File.rename("#{file}.tmp", file)
  end

  attr_writer :original_source
  def original_source
    @original_source || source
  end

  def is_method_for_block?
    @_st_gsmeth.__is_method_for_block
  end

  def is_def_method?
    self.original_source =~ /^\s+def\s/
  end
end

class Symbol
  def implementors
    ary = []
    ObjectSpace.each_object(Class) do |m|
      if m.instance_methods(true).include? self.to_s
        meth = m.instance_method(self)
      elsif m.methods(true).include? self.to_s
        meth = m.method(self)
      end
      ary << meth unless ary.include?(meth) || meth.nil?
    end
    ary
  end

  def senders
    ary = []
    ObjectSpace.each_object(Class) do |m|
      m.instance_methods(true).each do |selector|
        begin
          meth = m.instance_method(selector)
          unless ary.include?(meth) ||
              meth._st_gsmeth.__source_offset_first_send_of(self).nil?
            ary << meth
          end
        rescue Exception
          next
        end
      end

      m.methods(true).each do |selector|
        begin
          meth = m.method(selector)
          unless ary.include?(meth) ||
              meth._st_gsmeth.__source_offset_first_send_of(self).nil?
            ary << meth
          end
        rescue Exception
          next
        end
      end
    end
    ary
  end
end
