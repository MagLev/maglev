require File.expand_path('simple', File.dirname(__FILE__))

require 'rbconfig'

M = ENV['MAGLEV_HOME']
A = `uname -m`.chomp

test(RbConfig::CONFIG['libdir'],     "#{M}/lib",                'libdir')
test(RbConfig::CONFIG['rubylibdir'], "#{M}/lib/ruby/1.9",       'rubylibdir')
test(RbConfig::CONFIG['archdir'],    "#{M}/lib/ruby/1.9/#{A}",  'archdir')
test(RbConfig::CONFIG['topdir'],     "#{M}/lib/ruby/1.9/#{A}",  'topdir')

test(RbConfig::CONFIG['sitedir'],    "#{M}/lib/ruby/site_ruby", 'siteruby')
test(RbConfig::CONFIG['sitelibdir'],
  "#{M}/lib/ruby/site_ruby/1.9",
  'sitelibdir')

test(RbConfig::CONFIG['sitearchdir'], "#{M}/lib/ruby/site_ruby/1.9/#{A}",
  'sitearchdir')

test(RbConfig::CONFIG['vendordir'],    "#{M}/lib/vendor_ruby",
  'vendorruby')
test(RbConfig::CONFIG['vendorlibdir'], "#{M}/lib/vendor_ruby/1.9",
  'vendorlibdir')
test(RbConfig::CONFIG['vendorarchdir'],"#{M}/lib/vendor_ruby/1.9/#{A}",
  'vendorarchdir')

report
