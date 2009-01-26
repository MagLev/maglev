module Ramaze
  module Option
    module Merger

      # Pass it anything that may work as option: ENV, hashes, ARGV...
      # it will figure out how to get config from it and modify Global.
      def merge!(obj, &block)
        case obj
        when ENV
          merge_prefixed!(obj)
        else
          if obj.respond_to?(:each_pair)
            obj.each_pair do |key, value|
              self[key] = value
            end
          elsif obj.respond_to?(:each)
            merge_cli!(obj, &block)
          end
        end

        self
      end

      # Prefixed options, mostly useful for filtering out from ENV.
      # by default the prefix is case-insensitive and set to ramaze_ - that
      # works for keys like:
      #     $ RAMAZE_PORT=80 ruby start.rb
      #     $ ramaze_port=80 ruby start.rb
      #     $ Ramaze_Port=80 ruby start.rb
      def merge_prefixed!(obj, prefix = 'ramaze_')
        opts = {}
        obj.each do |key, value|
          next unless key =~ /^#{prefix}(.*)/i
          opts[$1.downcase] = value
        end
        merge! opts
      end

      # Only destructive for now, should non-destructive keep both Global and
      # the obj original?

      def merge_cli!(obj, &block)
        option_parser(&block).parse!(obj)
      end

      def option_parser(bin = 'ramaze')
        require 'optparse'
        require 'abbrev'

        OptionParser.new{|opt|
          ruby_version = "ruby #{RUBY_VERSION} (#{RUBY_RELEASE_DATE}) [#{RUBY_PLATFORM}]"
          ramaze_version = "Ramaze Version #{VERSION}"
          version = "#{ramaze_version}, on #{ruby_version}"

          opt.banner = "Usage: #{bin} start.rb [OPTIONS]"
          opt.define_head version

          yield(opt) if block_given?

          opt.separator ''
          opt.separator 'Global options, value in [] shows default.'

          Global.each do |key, value|
            long_option = "--#{key}"
            options = Global.trait.fetch(key, {})
            doc, cli, short = options.values_at(:doc, :cli, :short)
            short_option = "-#{short}" if short

            next unless cli or short

            case cli || value
            when Integer
              option = [short_option, "#{long_option} NUM", Integer, doc]
            when String, Symbol
              option = [short_option, "#{long_option} STRING", String, doc]
            when Array
              next unless cli
              cli = cli.map{|c| c.to_s }
              list = "  " << cli.join(', ')
              doc = "[#{value}] #{doc}"
              aliases = cli.abbrev
              option = [short_option, "#{long_option} CHOICE", cli, doc, list]
            else
              option = [short_option, long_option, doc]
            end

            option.delete(nil)

            opt.on(*option){|o| Ramaze::Global[key] = o }
          end

          opt.separator ''
          opt.separator 'Common options:'

          opt.on('-h', '--help', 'Show this message') do
            puts opt
            exit
          end

          opt.on('-v', '--version', "Show version") do
            puts version
            exit
          end
        }
      end
    end
  end
end
