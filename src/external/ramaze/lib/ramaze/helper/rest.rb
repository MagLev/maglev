module Ramaze
  module Helper
    module REST
      def self.included(klass)
        klass.class_eval do
          trait :REST => {
            'GET' => [], 'PUT' => [],
            'POST' => [], 'DELETE' => [],
            :any => [],
          }
          extend Indicate

          def self.method_added(name)
            name = name.to_s
            active = trait[:REST][:active] ||= :any
            trait[:REST][active] << name
          end
        end
      end

      module Indicate
        def on(http_method)
          hm = http_method.to_s.upcase
          trait[:REST][hm] = []
          trait[:REST][:active] = hm
        end

        (%w[GET PUT POST DELETE] << 'any').each do |http_method|
          define_method("on_#{http_method.downcase}") do |*args|
            if args.empty?
              on(http_method)
            else
              trait[:REST][http_method] += args.flatten
            end
          end
        end
      end
    end
  end
end

# POST /foo/bar
# methods_on_post[/foo/bar]
