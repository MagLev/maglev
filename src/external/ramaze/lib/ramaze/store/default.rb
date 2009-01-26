#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'yaml/store'

module Ramaze

  # Namespace for Stores in Ramaze

  module Store

    # A simple wrapper around YAML::Store

    class Default
      include Enumerable

      attr_accessor :db

      # create a new store with a filename

      def initialize filename = 'db.yaml'
        FileUtils.touch(filename)
        @db = YAML::Store.new(filename)
      end

      # yield a block in a transaction, identical to #db.transaction{}

      def transaction
        @db.transaction do
          yield
        end
      end

      # pass on all methods inside a transaction

      def method_missing(meth, *args, &block)
        read_only = (meth == :[])

        @db.transaction(read_only) do
          @db.send(meth, *args, &block)
        end
      end

      # the actual content of the store in YAML format

      def to_yaml
        dump(:x)
      end

      # loads the #to_yaml

      def original
        YAML.load(to_yaml)
      end

      # available keys of the store

      def keys
        (original || {}).keys
      end

      # is the Store empty? (no keys)

      def empty?
        keys.empty?
      end

      # nondestructive merge on #original

      def merge hash = {}
        original.merge(hash)
      end

      # destructive #merge

      def merge! hash = {}
        hash.each do |key, value|
          transaction do
            @db[key] = value
          end
        end

        original
      end

      # delete all entries

      def clear
        keys.each do |key|
          delete key
        end
      end

      # number of #keys

      def size
        keys.size
      end

      # Iterate over #original and pass the key and value to a block.

      def each
        original.each do |key, value|
          yield key, value
        end
      end
    end
  end
end
