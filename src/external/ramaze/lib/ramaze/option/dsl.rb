#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module Option
    class DSL
      attr_reader :holder

      def initialize(holder, &block)
        @holder = holder
        @config = {}
        instance_eval(&block) if block_given?
      end

      def o(doc, key, value, options = {})
        @config[key] = [doc, value, options]

        holder.add_option(key, value, complain = false)
        holder.trait[key] = options.merge(:doc => doc)
      end

      # to generate a new sorted source file from the config, was only used to
      # merge new and old DSL.
      def output
        @config.keys.sort_by{|k,v| k.to_s }.each do |key|
          doc, value, options = @config[key]
          puts "o %s," % doc.dump

          if options.empty?
            puts "  %p, %p" % [key, value]
          else
            format = options.map{|(k,v)| '%p => %p' % [k,v]}.join(', ')
            puts "  %p, %p, %s" % [key, value, format]
          end

          puts
        end
      end
    end

    def self.dsl(&block)
      Option::DSL.new(&block)
    end
  end
end
