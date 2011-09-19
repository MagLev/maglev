require 'maglev/objectlog'
require 'set'

class Thread
  # => GsNMethod
  primitive '__method_at', 'methodAt:'
  # => Fixnum
  primitive '__stack_depth', 'stackDepth'
  # Remove all frames above [Fixnum]
  primitive '__trim_stack_to_level', '_trimStackToLevel:'
  # Change temporary at level to value
  primitive '__frame_at_temp_named_put', '_frameAt:tempNamed:put:'
  # => Array
  #    with:
  #  1  gsMethod
  #  2  self
  #  4  selector
  #  5  quickStepPoint (offset into sourceOffsets)
  #  6  sourceOffsets (the points where each step would be at)
  #  7  argAndTempNames
  #  8  argAndTempValues (maybe smaller or larger than argAndTempNames)
  #  9  sourceString
  #  10 ipOffset
  #  11 markerOrException
  primitive '__gsi_debugger_detailed_report_at', '_gsiDebuggerDetailedReportAt:'
  # Stepping
  primitive '__step_over_in_frame', '_stepOverInFrame:'
  # Persistence conversions
  primitive 'convert_to_persistable_state', "convertToPersistableState"
  primitive 'convert_to_runnable_state', 'convertToRunnableState'
  primitive '__ar_stack', 'arStack'
  primitive '__client_data', '_clientData'
  #  Private.  Returns an Array describing the specified level in the receiver.
  #  aLevel == 1 is top of stack.  If aLevel is less than 1 or greater than
  #  stackDepth, returns nil.
  #  The result Array contains:
  #  offset item
  #  -----  -----
  #    0    gsMethod
  #    1    ipOffset
  #    2    frameOffset (zero-based)
  #    3    varContext
  #    4    saveProtectedMode
  #    5    markerOrException
  #    6    nil (not used)
  #    7    self (possibly nil in a ComplexBlock)
  #    8    argAndTempNames (an Array of Symbols or Strings)
  #    9    receiver
  #   10    arguments and temps, if any
  primitive '__frame_contents_at', '_frameContentsAt:'
  primitive '__run' , 'rubyRun'
  primitive '__wakeup', 'rubyResume'
  primitive '__value', 'value:'

  # Resuming a continuation is only possible through this method
  def run_callcc(ret_val = nil)
    __value(ret_val)
  end

  def run
    if in_persistable_state?
      raise RuntimeError, "You have to call #resume_continuation on the ObjectLogEntry for this Thread"
    end
    __run
  end

  def wakeup
    if in_persistable_state?
      raise RuntimeError, "You have to call #resume_continuation on the ObjectLogEntry for this Thread"
    end
    __wakeup
  end

  # Simple check whether the thread looks like as if it is in a persistable (i.e.
  # non-runnable) state
  def in_persistable_state?
    return true if thread_data.nil?
    thread_data.collect(&:class).collect(&:name).include? :RubyPersistableCompilerState
  end

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

  primitive '_report', '_reportOfSize:'
  def report(depth = self.__stack_depth)
    _report(depth)
  end

  class Frame
    attr_reader :method, :index, :thread

    def initialize(gsmethod, st_idx, thread)
      @method = ruby_method(gsmethod)
      @index = st_idx
      @thread = thread
    end

    # Frame actions

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

    # Frame report

    def receiver
      detailed_report[1]
    end

    def self
      detailed_report[2]
    end

    def selector
      detailed_report[3]
    end

    def step_offset
      detailed_report[4]
    end

    def args_and_temps
      names = detailed_report[6]
      values = detailed_report[7]
      (values.size - names.size).times {|i| names << ".t#{i+1}"}
      Hash[names.zip(values)]
    end

    def variable_context
      @thread.__frame_contents_at(@index)[3]
    end

    def inspect
      "#<Frame #{@index}: #{@method.in_class}##{@method.name} >> #{thread.inspect}>"
    end

    private
    def detailed_report
      @report ||= @thread.__gsi_debugger_detailed_report_at(@index)
    end

    def ruby_method(gsmethod)
      label = gsmethod.__name.to_s
      cls = gsmethod.__in_class
      if cls.instance_methods.include?(label)
        cls.instance_method(label.to_sym)
      else
        GsNMethodWrapper.new(gsmethod)
      end
    end
  end

  # The list of saved threads in the ObjectLog
  def self.saved_errors
    ObjectLog.errors
  end

  def raw_stack
    self.__ar_stack || begin # Force data into cache, if neccessary
                         self.stack
                         self.__ar_stack
                       end
  end

  def thread_data
    self.__client_data
  end

  def compiler_state
    self.__client_data.first
  end

  VariableContext = __resolve_smalltalk_global(:VariableContext)
  VariableContext.primitive '[]', 'at:'
  VariableContext.primitive '[]=', 'at:put:'
  VariableContext.primitive 'size', 'size'
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
  primitive '__the_non_meta_class', 'theNonMetaClass'

  # The namespace (lexical scope) in which the Module was defined
  def namespace
    if ts = __transient_namespace(1)
      return ts.parent
    end
  end

  def singleton_instance
    raise TypeError, "not a singleton class" unless self.singleton_class?
    if self.inspect =~ /^#<Class:.*>$/

    else
      raise NotImplementedError, "not implemented yet"
    end
  end

  def compile_method(source, selector = nil)
    meth_dict = instance_methods(false) + methods(false)
    if selector || (md = /^\s*def\s+(?:self\.)?([^;\( \n]+)/.match(source))
      selector = selector || md[1]
      begin
        method(selector).source!(source, true)
      rescue NameError, TypeError
        class_eval(source)
      end
    else
      class_eval(source)
    end
    new_meth_dict = instance_methods(false) + methods(false)
    new_method_selector = if new_meth_dict > meth_dict
                            (new_meth_dict - meth_dict).first
                          else
                            selector
                          end
    method(new_method_selector) if new_method_selector
  end

  # Traverse the Ruby namespace hierarchy and execute block for all classes
  # and modules.  Returns an IdentitySet of all classes and modules found.
  # Skips autoloads (i.e., does not trigger them and does not yield them to
  # the block).
  #
  # @param [Module] klass The Class or Module object to start traversal.
  #         Default is Object.
  #
  # @param [IdentitySet] rg The recursion guard used to prevent infinite
  #         loops; also used as return value.
  #
  # @return [IdentitySet] An IdentitySet of all the Classes and Modules
  #         registered in the Ruby namespace
  #
  def each_module(rg=IdentitySet.new, &block)
    unless rg.include?(self)
      rg.add self
      yield(self) if block
      self.constants.each do |c|
        unless self.autoload?(c)
          begin
            obj = self.const_get(c)
            obj.each_module(rg, &block) if Module === obj
          rescue Exception
            next
          end
        end
      end
    end
    rg
  end

  # Return an object named in the Ruby namespace.
  #
  # @param [String] name The name of the object. E.g., "Object",
  #         "Errno::EACCES", "Foo::Bar::Baz".
  #
  # @return [Object] the named object.
  #
  # @raise [NameError] if the name can't be found
  def find_in_namespace(name)
    name.split('::').inject(self) do |parent, name|
      obj = parent.const_get name
    end
  end

  primitive '__compile_method_category_environment_id', 'compileMethod:category:environmentId:'
end

class GsNMethod
  primitive '__is_method_for_block', 'isMethodForBlock'
  primitive '__source_string_for_block', '_sourceStringForBlock'
  primitive '__source_offsets', '_sourceOffsets'
  primitive '__source_offsets_of_sends', '_sourceOffsetsOfSends'
  primitive '__source_offset_first_send_of', '_sourceOffsetOfFirstSendOf:'
end

class GsNMethodWrapper
  def initialize(gsmethod); @gsmethod = gsmethod; end
  def step_offsets; @gsmethod.__source_offsets; end
  def send_offsets; @gsmethod.__source_offsets_of_sends; end
  def in_class; @gsmethod.__in_class; end
  def file; (@gsmethod.__source_location || [])[0]; end
  def line; (@gsmethod.__source_location || [])[1]; end
  def name; @gsmethod.__name; end

  def source
    if @gsmethod.__is_method_for_block
      @gsmethod.__source_string_for_block
    else
      @gsmethod.__source_string
    end
  end

  def source!(str, ignored)
    in_class.__compile_method_category_environment_id(str, '*maglev-webtools-unclassified', 1)
    self
  end

  def file_out(ignored); raise StandardError, "not an ordinary method def"; end
  def file=(ignored); raise TypeError, "cannot modify a Smalltalk method"; end
  def line=(ignored); raise TypeError, "cannot modify a Smalltalk method"; end
end

class UnboundMethod
  def step_offsets
    @_st_gsmeth.__source_offsets
  end

  def send_offsets
    @_st_gsmeth.__source_offsets_of_sends
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
    reload
  end

  def line=(num)
    raise TypeError, "cannot modifiy a block method" if is_method_for_block?
    @_st_gsmeth.__in_class.class_eval(source, file, num)
    reload
  end

  def source!(str, file_out = false)
    raise TypeError, "cannot modifiy a block method" if is_method_for_block?
    if file.nil? && line.nil? # Smalltalk method
      in_class.__compile_method_category_environment_id(str,
        '*maglev-webtools-unclassified', 1)
    else # Ruby method
      self.original_source = source
      self.in_class.class_eval(str, file, line)
      self.file_out(str) if file_out
    end
    reload
  end

  def file_out(source)
    if !is_def_method? || is_method_for_block?
      raise StandardError, "not an ordinary method def"
    end

    unless File.writable?(file)
      raise StandardError, "cannot write to method source file #{file}"
    end

    # Write a new file with updated contents
    original_contents = File.readlines(file)
    copy = File.open("#{file}.tmp", 'w+') do |f|
      f.write(original_contents[0...(line - 1)].join)
      f.write(original_contents[(line - 1)..-1].join.sub(original_source, source))
    end

    # Rename to original file
    File.rename("#{file}.tmp", file)
  end

  def reload
    @_st_gsmeth = in_class.__gs_method(self.name, true)
    self
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

  # Answers whether the method send the specified message.
  # Accepts the following options:
  #   args => how many arguments in the send (default: 0)
  #   splat => is there a splat argument (default: false)
  #   block => is there a block argument (default: false)
  #   keep => whether to just keep the selector as passed
  # @return true or false, depending on the result
  def sends_message?(string, options={})
    options = {:args => 0, :splat => false,
      :block => false, :keep => false}.merge(options)
    unless options[:keep]
      string = string.to_s + options[:args].to_s +
        (options[:splat] ? '*' : '_') +
        (options[:block] ? '&' : '_')
    end
    !@_st_gsmeth.__source_offset_first_send_of(string.to_sym).nil?
  end

  def ==(other)
    false unless other === self.class
    in_class == other.in_class && name == other.name
  end
end

class String
  def implementors
    ary = Set.new
    Object.each_module do |m|
      ary.add(m.instance_method(self)) if m.instance_methods(false).include? self
      ary.add(m.method(self)) if m.methods(false).include? self
    end
    ary
  end

  def senders
    ary = Set.new
    Object.each_module do |m|
      meths = m.instance_methods(false).collect {|n| m.instance_method(n) }
      meths += m.methods(false).collect {|n| m.method(n) }
      meths.each {|meth| ary.add(meth) if meth.sends_message?(self, :keep => true) }
    end
    ary
  end
end

class Maglev::System
  #--
  # Convenient access to available statmonitor statistics.
  # NOTE: Since really only the first two slots of cache statistics
  # are guaranteed, we just try the first 20 slots until giving up.
  #
  # @param slots pass this optional arg for the number of stat slots to return
  #        defaults to 20
  # @param determines whether the output is a sorted array with pairs of
  #        '[name, stats]', or a hash with 'name => stats' key-value pairs
  #
  # See the GS64 System Administration Guide, Appendix G: "statmonitor
  # and VSD reference" for details on the statistics system.
  #++
  def self.cache_statistics(slots = 20, sorted = false)
    ary = []
    descr = _cache_statistics_description[1..-1]
    slots.times {|i| ary << _cache_statistics(i) }
    ary = ary.compact.inject([]) do |array, statlist|
      array << [statlist.first, Hash[descr.zip(statlist[1..-1])]]
    end
    sorted ? ary : Hash[ary]
  end
end

class Object
  primitive 'find_all_references', 'findAllReferences'
  primitive 'find_all_references_in_memory', 'findReferencesInMemory'
end

class ObjectSpace::Repository
  primitive '__find_objs_connected_to', 'findObjsConnectedTo:'
  primitive '__findReferencePathToObjectsfindAllRefsprintToLog', 'findReferencePathToObjects:findAllRefs:printToLog:'

  def find_objs_connected_to(array_or_obj)
    __find_objs_connected_to([*array_or_obj])
  end

  def find_reference_path_to(array_or_obj, find_all = true, log = true)
    __findReferencePathToObjectsfindAllRefsprintToLog([*array_or_obj], find_all, log)
  end
end
