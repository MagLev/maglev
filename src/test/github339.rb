rs, ws = IO.pipe

arrays = IO.select([rs],[ws])
# Kernel.select and IO.select duplicate code ... I know this isn't
# ideal, but just test it for now
arrays += select([rs],[ws])

raise "#select should not return nil" if arrays.any?(&:nil?)
