module Ramaze
  class State
    def initialize
      @core = detect
    end

    def detect
      require 'fiber'
      @extract = :resume
      Ramaze::Fiber
    rescue LoadError
      @extract = :value
      ::Thread
    end

    def [](key)
      @core.current[key]
    end

    def []=(key, value)
      @core.current[key] = value
    end

    def key?(key)
      @core.current.key?(key)
    end

    def wrap(&block)
      @core.new(&block).send(@extract)
    end
  end

  module StateAccessor

    # Iterate over the names and yield accordingly.
    # names are either objects responding to #to_sym or hashes.
    #
    # It's only used within this module for abstractin-purposes.
    # Usage below.
    def self.each(*names)
      names.each do |name|
        if name.respond_to?(:to_hash)
          name.to_hash.each do |key, meth|
            key, meth = key.to_sym, meth.to_sym
            yield key, meth
          end
        else
          key = meth = name.to_sym
          yield key, meth
        end
      end
    end

    # state_writer and state_reader, initializer is a block that may be given
    # and its result will be the new value in case the reader was never called
    # before or the value wasn't set before.
    def state_accessor(*names, &initializer)
      state_writer(*names)
      state_reader(*names, &initializer)
    end

    # Simple writer accessor to STATE[key]=
    def state_writer(*names)
      StateAccessor.each(*names) do |key, meth|
        define_method("#{meth}="){|obj| STATE[key] = obj }
      end
    end

    # Reader accessor for STATE[key]
    def state_reader(*names, &initializer)
      StateAccessor.each(*names) do |key, meth|
        if initializer
          define_method(meth) do
            unless STATE.key?(key)
              STATE[key] = instance_eval(&initializer)
            else
              STATE[key]
            end
          end
        else
          define_method(meth){ STATE[key] }
        end
      end
    end
  end
end
