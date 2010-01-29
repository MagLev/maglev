require 'libpsych'
require 'psych/parser'

module Psych
  def self.libyaml_version
    version_info = FFI::MemoryPointer.new(:int, 3);
    LibPsych.libyaml_version(version_info)
    [version_info[0], version_info[1], version_info[2]]
  end
end
