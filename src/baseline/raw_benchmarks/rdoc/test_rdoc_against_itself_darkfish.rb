require 'fileutils'
require './rdoc_bm_helper'
# Bench.run [1] do |n|
[1].map do |n|
  # run rdoc against itself
  go 'rdp-rdoc-2.4.6'
end
