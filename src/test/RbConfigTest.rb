require File.expand_path('simple', File.dirname(__FILE__))

require 'rbconfig'

M = ENV['MAGLEV_HOME']
A = `uname -m`

test(RbConfig::CONFIG['rubylibdir'],  "#{M}/1.8",      'rubylibdir')
test(RbConfig::CONFIG['archdir'],     "#{M}/1.8/#{A}", 'archdir')
test(RbConfig::CONFIG['topdir'],      "#{M}/1.8/#{A}", 'topdir')
test(RbConfig::CONFIG['datadir'],     "#{M}/data",     'datadir')

test(RbConfig::CONFIG['sitedir'],     "#{M}/site_ruby",          'siteruby')
test(RbConfig::CONFIG['sitelibdir'],  "#{M}/site_ruby/1.8",      'sitelibdir')
test(RbConfig::CONFIG['sitearchdir'], "#{M}/site_ruby/1.8/#{A}", 'sitearchdir')

test(RbConfig::CONFIG['vendordir'],    "#{M}/vendor_ruby",          'vendorruby')
test(RbConfig::CONFIG['vendorlibdir'], "#{M}/vendor_ruby/1.8",      'vendorlibdir')
test(RbConfig::CONFIG['vendorarchdir'],"#{M}/vendor_ruby/1.8/#{A}", 'vendorarchdir')

report
