require 'bundler/fetcher'

# See RubyGems default/maglev.rb
module Bundler
  class Fetcher
    Gem.maglev_override_info
    alias_method :original_fetch_dependency_remote_specs, :fetch_dependency_remote_specs

    def fetch_dependency_remote_specs(gem_names)
      gem_names += gem_names.collect {|name| "#{name}#{Gem::MAGLEV_POSTFIX}" }
      spec_list, deps_list = original_fetch_dependency_remote_specs(gem_names)

      spec_list.collect! do |s|
        if s.first.end_with? Gem::MAGLEV_POSTFIX
          [s.first[0...-Gem::MAGLEV_POSTFIX.size], s[1], "maglev", s[3]]
        else
          s
        end
      end
      [spec_list, deps_list]
    end
  end
end
