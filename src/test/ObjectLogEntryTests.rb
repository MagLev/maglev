# Test the ObjectLogEntry
require File.expand_path('simple', File.dirname(__FILE__))

require 'maglev/objectlog'

# Test setup
ole = ObjectLogEntry.debug("A Message", [:a])
ole.add_to_log
log = ObjectLogEntry.object_log

# Test assertions
test(ole.class,        ObjectLogEntry, "1: Constructor")
test(log.equal?(nil),  false,          "2: non-nil log")
test(log.include?(ole), true,           "3: log entry was added")

report
