#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.
require 'yaml/store'

module Ramaze

  # Cache based on _whys YAML::Store, which uses PStore to serialize objects
  # as YAML in a thread-safe manner.

  class YAMLStoreCache

    attr_accessor :file

    # create a new YAML::Store with the given file (which will be created if it
    # is not already there).

    def initialize(file = 'cache.yaml')
      @file = file
      @cache = YAML::Store.new(file)
    end

    # return the values for given keys.

    def values_at(*keys)
      transaction do |y|
        keys.map{|k| y[k]}
      end
    end

    # Loads @file into memory via YAML::load_file

    def underlying_yaml
      YAML.load_file(@file)
    end

    # clears the YAML::Store based cache, by emptying the YAML file.

    def clear
      transaction do |y|
        File.open(@file, 'w+'){|f| f.puts({}.to_yaml)}
      end
    end

    # Deletes the key from YAML::Store based cache.

    def delete(key)
      transaction do |y|
        y.delete(key)
      end
    end

    # just a helper to use transactions.

    def transaction(&block)
      @cache.transaction do
        yield(@cache)
      end
    end

    # catch everything else and use a transaction to send it.

    def method_missing(*args, &block)
      transaction do |y|
        y.send(*args, &block)
      end
    end
  end
end
