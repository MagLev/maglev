class CZstream
    # CZstream is not intended for use outside of the Zlib module

    class_primitive_nobridge 'open_read', 'openRead:errorClass:'
    class_primitive_nobridge 'open_write', 'openWrite:errorClass:comprLevel:'
    class_primitive_nobridge 'allocate', '_basicNew'

    primitive_nobridge 'read', 'read:'
    primitive_nobridge 'read_header', 'header'
    primitive_nobridge 'write_header', 'writeHeader:'
    primitive_nobridge 'at_eof', 'atEnd'
    primitive_nobridge 'total_out', 'position'
    primitive_nobridge 'close', 'close'
    primitive_nobridge 'flush', 'flush'
    primitive_nobridge 'write', 'rubyWrite:count:'

    primitive_nobridge '_open', 'open:io:errorClass:comprLevel:'

    # Finishes the stream and flushes output buffer.
    def finish
    end
end

module Zlib

  ZStream = ::CZstream

end
