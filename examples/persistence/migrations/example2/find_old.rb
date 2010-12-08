#require '../lib/core_ext'

# Since we no longer have a nice handle to the old class (the 'Point'
# constant now references the 2.0.0 version of the class), we find an
# old-style object and follow its class pointer.

old_point = Maglev::PERSISTENT_ROOT[:points].detect { |p| p.class::VERSION == "1.0.0" }
old_class = old_point.class

raise "Couldn't find old version of class" if old_class.nil?

Maglev::PERSISTENT_ROOT[:old_points] = old_class.all_instances
Maglev.commit_transaction
