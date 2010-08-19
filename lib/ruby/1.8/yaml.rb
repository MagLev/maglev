#  MagLev uses the ruby code from Psych, plus our own FFI wrapper, to
#  implement YAML.  The EngineManager is copied from MRI 1.9.2, with a few
#  tweaks since MagLev only supports the psych engine.
#
module YAML
  class EngineManager # :nodoc:
    attr_reader :yamler

    def initialize
      @yamler = nil
    end

    def syck?
      'syck' == @yamler
    end

    def yamler= engine
      raise(ArgumentError, "bad engine") unless engine == 'psych'

      require engine

      Object.class_eval <<-eorb, __FILE__, __LINE__ + 1
        remove_const 'YAML'
        YAML = #{engine.capitalize}
        remove_method :to_yaml
        alias :to_yaml :#{engine}_to_yaml
      eorb

      @yamler = engine
      engine
    end
  end

  ENGINE = YAML::EngineManager.new
end

require 'psych'
module Psych
  ENGINE = YAML::ENGINE
end

YAML = Psych
YAML::ENGINE.yamler = 'psych'
