#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'ramaze/trinity'

module Ramaze

  # A module used by the Templates and the Controllers
  # it provides both Ramaze::Trinity (request/response/session)
  # and also a helper method, look below for more information about it

  module Helper
    LOOKUP = Set.new
    PATH = ['']
    path = File.expand_path("#{BASEDIR}/../spec/ramaze/helper/")
    trait :ignore => [
      /#{Regexp.escape(path)}\//
    ]

    module Methods
      def self.included other
        other.send :extend, Trinity
        other.send :extend, Methods
        other.send :include, Trinity
        super
      end

      def self.extend_object other
        other.send :extend, Trinity
        super
      end

      # This loads the helper-files from /ramaze/helper/helpername.rb and
      # includes it into Ramaze::Template (or wherever it is called)
      #
      # Usage:
      #   helper :redirect, :link

      def helper(*syms)
        syms.each do |sym|
          name = sym.to_s
          if mod = find_helper(name)
            use_helper(mod)
          else
            if require_helper(name)
              redo
            else
              raise LoadError, "helper #{name} not found"
            end
          end
        end
      end

      private

      # returns the Ramaze::Helper::Name Module Constant if exists.

      def find_helper(name)
        name = name.to_s.camel_case
        ramaze_helper_consts = ::Ramaze::Helper.constants.grep(/^#{name}$/i)
        if mod_name = ramaze_helper_consts.first
          ::Ramaze::Helper.const_get(mod_name)
        end
      end

      # Loads helper from /lib/ramaze/helper/name.(so, bundle, rb)
      # raises LoadError if helper not found.

      def require_helper(name)
        paths = (PATH + [Global.root, "#{BASEDIR}/ramaze"]).join(',')
        glob = "{#{paths}}/helper/#{name}.{so,bundle,rb}"
        files = Dir[glob]
        ignore = Helper.trait[:ignore]
        files.reject!{|f| ignore.any?{|i| f =~ i }}
        raise LoadError, "file for #{name} not found" unless file = files.first
        require(file)
      end

      # injects the helper via include and extend, takes Constant as argument.

      def use_helper(mod)
        include mod
        extend mod
      end
    end
  end
end
