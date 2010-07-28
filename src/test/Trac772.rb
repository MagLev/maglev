# Distilled from Rails/Rack routing
#
# MagLev was getting nil from a hash if the key was a rack mount splitting key.
#
# Notes:
# 1. k.hash was 0 in maglev, but non zero in MRI for a ...Splitting:Key.
# 2. Both MagLev and MRI are at the following Gem versions:
#      $ maglev-gem list rack
#      rack (1.1.0)
#      rack-mount (0.6.9)
#      rack-test (0.5.4)
require 'rubygems'
require 'rack/mount'

h = { }
k = Rack::Mount::Analysis::Splitting::Key.new(:path_info, 0, /\.|\// )

h[k] = "posts"
v = h[k]

raise "FAIL" unless v == "posts"
h[:request_method] = /GET/

v = h[k]
raise "FAIL" unless v == "posts"
true
