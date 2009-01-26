#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require(File.join(File.dirname(__FILE__), 'blankslate'))

# Basically an Set, but with Order, ain't that obivous?
class OrderedSet < BlankSlate
  def self.[](*args)
    new(*args)
  end

  # Create new instances, optionally pass the first set
  def initialize(*args)
    if args.size == 1
      @set = args.shift
    else
      @set = *args
    end

    @set ||= []
    @set = [@set] unless ::Array === @set
    @set.uniq!
  end

  %w[ push unshift << ].each do |meth|
    class_eval %[
      def #{meth} *args
        @set.delete(*args)
        @set.#{meth}(*args)
      end
    ]
  end

  def []= *args
    @set.map! do |e|
      if ::Array === args.last
        args.last.include?(e) ? nil : e
      else
        args.last == e ? nil : e
      end
    end
    @set.__send__(:[]=, *args)
    @set.compact!
  end

  # Delegate everything, but controlled, keep elements unique.
  # Warning, this is not really atomic.
  def method_missing(meth, *args, &block)
    @set.__send__(meth, *args, &block)
  end
end
