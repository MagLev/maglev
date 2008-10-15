# This module was used to gather information on the classes, modules and
# methods defined in MRI.  We need it so we can distinguish MRI methods
# from methods added to core classes by Rails, Gems etc.  This module now
# mostly just holds two hashes-of-hashes. Each hash maps a core Class or
# Module to its methods.  The rest of the code was used to generate the
# hashes, but isn't needed any more.
module MRIInfo
  # These are the classes we care about
  CORE = [ Array, Bignum, Binding, Class, Comparable, Continuation, Dir,
           Enumerable, Errno, Exception, FalseClass, File, File::Stat,
           FileTest, Fixnum, Float, GC, Hash, Integer, IO, Kernel,
           Marshal, MatchData, Math, Method, Module, NilClass, Numeric,
           Object, ObjectSpace, Proc, Process, Process::GID, Process::Status,
           Process::Sys, Process::UID, Range, Regexp, Signal, String, Struct,
           Struct::Tms, Symbol, Thread, ThreadGroup, Time, TrueClass,
           UnboundMethod ]

  # This data was generated once using:
  # MRIInfo.print_hash_of_hashes MRIInfo.collect_instance_methods(MRIInfo::CORE)
  MRI_INSTANCE_METHODS = {
    "Array" => {"&" => 1, "*" => 1, "+" => 1, "-" => 1, "<<" => 1, "<=>" => 1, "==" => 1, "[]" => 1, "[]=" => 1, "assoc" => 1, "at" => 1, "clear" => 1, "collect" => 1, "collect!" => 1, "compact" => 1, "compact!" => 1, "concat" => 1, "delete" => 1, "delete_at" => 1, "delete_if" => 1, "each" => 1, "each_index" => 1, "empty?" => 1, "eql?" => 1, "fetch" => 1, "fill" => 1, "first" => 1, "flatten" => 1, "flatten!" => 1, "frozen?" => 1, "hash" => 1, "include?" => 1, "index" => 1, "indexes" => 1, "indices" => 1, "insert" => 1, "inspect" => 1, "join" => 1, "last" => 1, "length" => 1, "map" => 1, "map!" => 1, "nitems" => 1, "pack" => 1, "pop" => 1, "push" => 1, "rassoc" => 1, "reject" => 1, "reject!" => 1, "replace" => 1, "reverse" => 1, "reverse!" => 1, "reverse_each" => 1, "rindex" => 1, "select" => 1, "shift" => 1, "size" => 1, "slice" => 1, "slice!" => 1, "sort" => 1, "sort!" => 1, "to_a" => 1, "to_ary" => 1, "to_s" => 1, "transpose" => 1, "uniq" => 1, "uniq!" => 1, "unshift" => 1, "values_at" => 1, "zip" => 1, "|" => 1, },
    "Bignum" => {"%" => 1, "&" => 1, "*" => 1, "**" => 1, "+" => 1, "-" => 1, "-@" => 1, "/" => 1, "<<" => 1, "<=>" => 1, "==" => 1, ">>" => 1, "[]" => 1, "^" => 1, "abs" => 1, "coerce" => 1, "div" => 1, "divmod" => 1, "eql?" => 1, "hash" => 1, "modulo" => 1, "quo" => 1, "remainder" => 1, "size" => 1, "to_f" => 1, "to_s" => 1, "|" => 1, "~" => 1, },
    "Binding" => {"clone" => 1, "dup" => 1, },
    "Class" => {"allocate" => 1, "new" => 1, "superclass" => 1, },
    "Comparable" => {"<" => 1, "<=" => 1, "==" => 1, ">" => 1, ">=" => 1, "between?" => 1, },
    "Continuation" => {"[]" => 1, "call" => 1, },
    "Dir" => {"close" => 1, "each" => 1, "path" => 1, "pos" => 1, "pos=" => 1, "read" => 1, "rewind" => 1, "seek" => 1, "tell" => 1, },
    "Enumerable" => {"all?" => 1, "any?" => 1, "collect" => 1, "detect" => 1, "each_with_index" => 1, "entries" => 1, "find" => 1, "find_all" => 1, "grep" => 1, "include?" => 1, "inject" => 1, "map" => 1, "max" => 1, "member?" => 1, "min" => 1, "partition" => 1, "reject" => 1, "select" => 1, "sort" => 1, "sort_by" => 1, "to_a" => 1, "zip" => 1, },
    "Errno" => {},
    "Exception" => {"backtrace" => 1, "exception" => 1, "inspect" => 1, "message" => 1, "set_backtrace" => 1, "to_s" => 1, "to_str" => 1, },
    "FalseClass" => {"&" => 1, "^" => 1, "to_s" => 1, "|" => 1, },
    "File" => {"atime" => 1, "chmod" => 1, "chown" => 1, "ctime" => 1, "flock" => 1, "lstat" => 1, "mtime" => 1, "path" => 1, "truncate" => 1, },
    "File::Stat" => {"<=>" => 1, "atime" => 1, "blksize" => 1, "blockdev?" => 1, "blocks" => 1, "chardev?" => 1, "ctime" => 1, "dev" => 1, "dev_major" => 1, "dev_minor" => 1, "directory?" => 1, "executable?" => 1, "executable_real?" => 1, "file?" => 1, "ftype" => 1, "gid" => 1, "grpowned?" => 1, "ino" => 1, "inspect" => 1, "mode" => 1, "mtime" => 1, "nlink" => 1, "owned?" => 1, "pipe?" => 1, "rdev" => 1, "rdev_major" => 1, "rdev_minor" => 1, "readable?" => 1, "readable_real?" => 1, "setgid?" => 1, "setuid?" => 1, "size" => 1, "size?" => 1, "socket?" => 1, "sticky?" => 1, "symlink?" => 1, "uid" => 1, "writable?" => 1, "writable_real?" => 1, "zero?" => 1, },
    "FileTest" => {},
    "Fixnum" => {"%" => 1, "&" => 1, "*" => 1, "**" => 1, "+" => 1, "-" => 1, "-@" => 1, "/" => 1, "<" => 1, "<<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, ">" => 1, ">=" => 1, ">>" => 1, "[]" => 1, "^" => 1, "abs" => 1, "div" => 1, "divmod" => 1, "id2name" => 1, "modulo" => 1, "quo" => 1, "size" => 1, "to_f" => 1, "to_s" => 1, "to_sym" => 1, "zero?" => 1, "|" => 1, "~" => 1, },
    "Float" => {"%" => 1, "*" => 1, "**" => 1, "+" => 1, "-" => 1, "-@" => 1, "/" => 1, "<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, ">" => 1, ">=" => 1, "abs" => 1, "ceil" => 1, "coerce" => 1, "divmod" => 1, "eql?" => 1, "finite?" => 1, "floor" => 1, "hash" => 1, "infinite?" => 1, "modulo" => 1, "nan?" => 1, "round" => 1, "to_f" => 1, "to_i" => 1, "to_int" => 1, "to_s" => 1, "truncate" => 1, "zero?" => 1, },
    "GC" => {"garbage_collect" => 1, },
    "Hash" => {"==" => 1, "[]" => 1, "[]=" => 1, "clear" => 1, "default" => 1, "default=" => 1, "default_proc" => 1, "delete" => 1, "delete_if" => 1, "each" => 1, "each_key" => 1, "each_pair" => 1, "each_value" => 1, "empty?" => 1, "fetch" => 1, "has_key?" => 1, "has_value?" => 1, "include?" => 1, "index" => 1, "indexes" => 1, "indices" => 1, "inspect" => 1, "invert" => 1, "key?" => 1, "keys" => 1, "length" => 1, "member?" => 1, "merge" => 1, "merge!" => 1, "rehash" => 1, "reject" => 1, "reject!" => 1, "replace" => 1, "select" => 1, "shift" => 1, "size" => 1, "sort" => 1, "store" => 1, "to_a" => 1, "to_hash" => 1, "to_s" => 1, "update" => 1, "value?" => 1, "values" => 1, "values_at" => 1, },
    "IO" => {"<<" => 1, "binmode" => 1, "close" => 1, "close_read" => 1, "close_write" => 1, "closed?" => 1, "each" => 1, "each_byte" => 1, "each_line" => 1, "eof" => 1, "eof?" => 1, "fcntl" => 1, "fileno" => 1, "flush" => 1, "fsync" => 1, "getc" => 1, "gets" => 1, "inspect" => 1, "ioctl" => 1, "isatty" => 1, "lineno" => 1, "lineno=" => 1, "pid" => 1, "pos" => 1, "pos=" => 1, "print" => 1, "printf" => 1, "putc" => 1, "puts" => 1, "read" => 1, "read_nonblock" => 1, "readchar" => 1, "readline" => 1, "readlines" => 1, "readpartial" => 1, "reopen" => 1, "rewind" => 1, "seek" => 1, "stat" => 1, "sync" => 1, "sync=" => 1, "sysread" => 1, "sysseek" => 1, "syswrite" => 1, "tell" => 1, "to_i" => 1, "to_io" => 1, "tty?" => 1, "ungetc" => 1, "write" => 1, "write_nonblock" => 1, },
    "Integer" => {"ceil" => 1, "chr" => 1, "downto" => 1, "floor" => 1, "integer?" => 1, "next" => 1, "round" => 1, "succ" => 1, "times" => 1, "to_i" => 1, "to_int" => 1, "truncate" => 1, "upto" => 1, },
    "Kernel" => {"==" => 1, "===" => 1, "=~" => 1, "__id__" => 1, "__send__" => 1, "class" => 1, "clone" => 1, "display" => 1, "dup" => 1, "eql?" => 1, "equal?" => 1, "extend" => 1, "freeze" => 1, "frozen?" => 1, "hash" => 1, "id" => 1, "inspect" => 1, "instance_eval" => 1, "instance_of?" => 1, "instance_variable_defined?" => 1, "instance_variable_get" => 1, "instance_variable_set" => 1, "instance_variables" => 1, "is_a?" => 1, "kind_of?" => 1, "method" => 1, "methods" => 1, "nil?" => 1, "object_id" => 1, "private_methods" => 1, "protected_methods" => 1, "public_methods" => 1, "respond_to?" => 1, "send" => 1, "singleton_methods" => 1, "taint" => 1, "tainted?" => 1, "to_a" => 1, "to_s" => 1, "type" => 1, "untaint" => 1, },
    "Marshal" => {},
    "MatchData" => {"[]" => 1, "begin" => 1, "captures" => 1, "end" => 1, "inspect" => 1, "length" => 1, "offset" => 1, "post_match" => 1, "pre_match" => 1, "select" => 1, "size" => 1, "string" => 1, "to_a" => 1, "to_s" => 1, "values_at" => 1, },
    "Math" => {},
    "Method" => {"==" => 1, "[]" => 1, "arity" => 1, "call" => 1, "clone" => 1, "inspect" => 1, "to_proc" => 1, "to_s" => 1, "unbind" => 1, },
    "Module" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "freeze" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "to_s" => 1, },
    "NilClass" => {"&" => 1, "^" => 1, "inspect" => 1, "nil?" => 1, "to_a" => 1, "to_f" => 1, "to_i" => 1, "to_s" => 1, "|" => 1, },
    "Numeric" => {"+@" => 1, "-@" => 1, "<=>" => 1, "abs" => 1, "ceil" => 1, "coerce" => 1, "div" => 1, "divmod" => 1, "eql?" => 1, "floor" => 1, "integer?" => 1, "modulo" => 1, "nonzero?" => 1, "quo" => 1, "remainder" => 1, "round" => 1, "singleton_method_added" => 1, "step" => 1, "to_int" => 1, "truncate" => 1, "zero?" => 1, },
    "Object" => {},
    "ObjectSpace" => {},
    "Proc" => {"==" => 1, "[]" => 1, "arity" => 1, "binding" => 1, "call" => 1, "clone" => 1, "dup" => 1, "to_proc" => 1, "to_s" => 1, },
    "Process" => {},
    "Process::GID" => {},
    "Process::Status" => {"&" => 1, "==" => 1, ">>" => 1, "coredump?" => 1, "exited?" => 1, "exitstatus" => 1, "inspect" => 1, "pid" => 1, "signaled?" => 1, "stopped?" => 1, "stopsig" => 1, "success?" => 1, "termsig" => 1, "to_i" => 1, "to_int" => 1, "to_s" => 1, },
    "Process::Sys" => {},
    "Process::UID" => {},
    "Range" => {"==" => 1, "===" => 1, "begin" => 1, "each" => 1, "end" => 1, "eql?" => 1, "exclude_end?" => 1, "first" => 1, "hash" => 1, "include?" => 1, "inspect" => 1, "last" => 1, "member?" => 1, "step" => 1, "to_s" => 1, },
    "Regexp" => {"==" => 1, "===" => 1, "=~" => 1, "casefold?" => 1, "eql?" => 1, "hash" => 1, "inspect" => 1, "kcode" => 1, "match" => 1, "options" => 1, "source" => 1, "to_s" => 1, "~" => 1, },
    "Signal" => {},
    "String" => {"%" => 1, "*" => 1, "+" => 1, "<<" => 1, "<=>" => 1, "==" => 1, "=~" => 1, "[]" => 1, "[]=" => 1, "capitalize" => 1, "capitalize!" => 1, "casecmp" => 1, "center" => 1, "chomp" => 1, "chomp!" => 1, "chop" => 1, "chop!" => 1, "concat" => 1, "count" => 1, "crypt" => 1, "delete" => 1, "delete!" => 1, "downcase" => 1, "downcase!" => 1, "dump" => 1, "each" => 1, "each_byte" => 1, "each_line" => 1, "empty?" => 1, "eql?" => 1, "gsub" => 1, "gsub!" => 1, "hash" => 1, "hex" => 1, "include?" => 1, "index" => 1, "insert" => 1, "inspect" => 1, "intern" => 1, "length" => 1, "ljust" => 1, "lstrip" => 1, "lstrip!" => 1, "match" => 1, "next" => 1, "next!" => 1, "oct" => 1, "replace" => 1, "reverse" => 1, "reverse!" => 1, "rindex" => 1, "rjust" => 1, "rstrip" => 1, "rstrip!" => 1, "scan" => 1, "size" => 1, "slice" => 1, "slice!" => 1, "split" => 1, "squeeze" => 1, "squeeze!" => 1, "strip" => 1, "strip!" => 1, "sub" => 1, "sub!" => 1, "succ" => 1, "succ!" => 1, "sum" => 1, "swapcase" => 1, "swapcase!" => 1, "to_f" => 1, "to_i" => 1, "to_s" => 1, "to_str" => 1, "to_sym" => 1, "tr" => 1, "tr!" => 1, "tr_s" => 1, "tr_s!" => 1, "unpack" => 1, "upcase" => 1, "upcase!" => 1, "upto" => 1, },
    "Struct" => {"==" => 1, "[]" => 1, "[]=" => 1, "each" => 1, "each_pair" => 1, "eql?" => 1, "hash" => 1, "inspect" => 1, "length" => 1, "members" => 1, "select" => 1, "size" => 1, "to_a" => 1, "to_s" => 1, "values" => 1, "values_at" => 1, },
    "Struct::Tms" => {"cstime" => 1, "cstime=" => 1, "cutime" => 1, "cutime=" => 1, "stime" => 1, "stime=" => 1, "utime" => 1, "utime=" => 1, },
    "Symbol" => {"===" => 1, "id2name" => 1, "inspect" => 1, "to_i" => 1, "to_int" => 1, "to_s" => 1, "to_sym" => 1, },
    "Thread" => {"[]" => 1, "[]=" => 1, "abort_on_exception" => 1, "abort_on_exception=" => 1, "alive?" => 1, "exit" => 1, "exit!" => 1, "group" => 1, "inspect" => 1, "join" => 1, "key?" => 1, "keys" => 1, "kill" => 1, "kill!" => 1, "priority" => 1, "priority=" => 1, "raise" => 1, "run" => 1, "safe_level" => 1, "status" => 1, "stop?" => 1, "terminate" => 1, "terminate!" => 1, "value" => 1, "wakeup" => 1, },
    "ThreadGroup" => {"add" => 1, "enclose" => 1, "enclosed?" => 1, "list" => 1, },
    "Time" => {"+" => 1, "-" => 1, "<=>" => 1, "_dump" => 1, "asctime" => 1, "ctime" => 1, "day" => 1, "dst?" => 1, "eql?" => 1, "getgm" => 1, "getlocal" => 1, "getutc" => 1, "gmt?" => 1, "gmt_offset" => 1, "gmtime" => 1, "gmtoff" => 1, "hash" => 1, "hour" => 1, "inspect" => 1, "isdst" => 1, "localtime" => 1, "mday" => 1, "min" => 1, "mon" => 1, "month" => 1, "sec" => 1, "strftime" => 1, "succ" => 1, "to_a" => 1, "to_f" => 1, "to_i" => 1, "to_s" => 1, "tv_sec" => 1, "tv_usec" => 1, "usec" => 1, "utc" => 1, "utc?" => 1, "utc_offset" => 1, "wday" => 1, "yday" => 1, "year" => 1, "zone" => 1, },
    "TrueClass" => {"&" => 1, "^" => 1, "to_s" => 1, "|" => 1, },
    "UnboundMethod" => {"==" => 1, "arity" => 1, "bind" => 1, "clone" => 1, "inspect" => 1, "to_s" => 1, },
  }

  # This data was generated once using:
  # MRIInfo.print_hash_of_hashes MRIInfo.collect_class_methods(MRIInfo::CORE)
  MRI_CLASS_METHODS = {
    "Array" => {"[]" => 1, "allocate" => 1, "new" => 1, "superclass" => 1, },
    "Bignum" => {"allocate" => 1, "induced_from" => 1, "superclass" => 1, },
    "Binding" => {"allocate" => 1, "superclass" => 1, },
    "Class" => {"allocate" => 1, "constants" => 1, "nesting" => 1, "new" => 1, "superclass" => 1, },
    "Comparable" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "freeze" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "to_s" => 1, },
    "Continuation" => {"allocate" => 1, "superclass" => 1, },
    "Dir" => {"[]" => 1, "allocate" => 1, "chdir" => 1, "chroot" => 1, "delete" => 1, "entries" => 1, "foreach" => 1, "getwd" => 1, "glob" => 1, "mkdir" => 1, "new" => 1, "open" => 1, "pwd" => 1, "rmdir" => 1, "superclass" => 1, "unlink" => 1, },
    "Enumerable" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "freeze" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "to_s" => 1, },
    "Errno" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "freeze" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "to_s" => 1, },
    "Exception" => {"allocate" => 1, "exception" => 1, "new" => 1, "superclass" => 1, },
    "FalseClass" => {"allocate" => 1, "superclass" => 1, },
    "File" => {"allocate" => 1, "atime" => 1, "basename" => 1, "blockdev?" => 1, "chardev?" => 1, "chmod" => 1, "chown" => 1, "ctime" => 1, "delete" => 1, "directory?" => 1, "dirname" => 1, "executable?" => 1, "executable_real?" => 1, "exist?" => 1, "exists?" => 1, "expand_path" => 1, "extname" => 1, "file?" => 1, "fnmatch" => 1, "fnmatch?" => 1, "for_fd" => 1, "foreach" => 1, "ftype" => 1, "grpowned?" => 1, "identical?" => 1, "join" => 1, "lchmod" => 1, "lchown" => 1, "link" => 1, "lstat" => 1, "mtime" => 1, "new" => 1, "open" => 1, "owned?" => 1, "pipe" => 1, "pipe?" => 1, "popen" => 1, "read" => 1, "readable?" => 1, "readable_real?" => 1, "readlines" => 1, "readlink" => 1, "rename" => 1, "select" => 1, "setgid?" => 1, "setuid?" => 1, "size" => 1, "size?" => 1, "socket?" => 1, "split" => 1, "stat" => 1, "sticky?" => 1, "superclass" => 1, "symlink" => 1, "symlink?" => 1, "sysopen" => 1, "truncate" => 1, "umask" => 1, "unlink" => 1, "utime" => 1, "writable?" => 1, "writable_real?" => 1, "zero?" => 1, },
    "File::Stat" => {"allocate" => 1, "new" => 1, "superclass" => 1, },
    "FileTest" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "blockdev?" => 1, "chardev?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "directory?" => 1, "executable?" => 1, "executable_real?" => 1, "exist?" => 1, "exists?" => 1, "file?" => 1, "freeze" => 1, "grpowned?" => 1, "identical?" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "owned?" => 1, "pipe?" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "readable?" => 1, "readable_real?" => 1, "setgid?" => 1, "setuid?" => 1, "size" => 1, "size?" => 1, "socket?" => 1, "sticky?" => 1, "symlink?" => 1, "to_s" => 1, "writable?" => 1, "writable_real?" => 1, "zero?" => 1, },
    "Fixnum" => {"allocate" => 1, "induced_from" => 1, "superclass" => 1, },
    "Float" => {"allocate" => 1, "induced_from" => 1, "superclass" => 1, },
    "GC" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "disable" => 1, "enable" => 1, "freeze" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "start" => 1, "to_s" => 1, },
    "Hash" => {"[]" => 1, "allocate" => 1, "new" => 1, "superclass" => 1, },
    "IO" => {"allocate" => 1, "for_fd" => 1, "foreach" => 1, "new" => 1, "open" => 1, "pipe" => 1, "popen" => 1, "read" => 1, "readlines" => 1, "select" => 1, "superclass" => 1, "sysopen" => 1, },
    "Integer" => {"allocate" => 1, "induced_from" => 1, "superclass" => 1, },
    "Kernel" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "Array" => 1, "Float" => 1, "Integer" => 1, "String" => 1, "`" => 1, "abort" => 1, "ancestors" => 1, "at_exit" => 1, "autoload" => 1, "autoload?" => 1, "binding" => 1, "block_given?" => 1, "callcc" => 1, "caller" => 1, "catch" => 1, "chomp" => 1, "chomp!" => 1, "chop" => 1, "chop!" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "eval" => 1, "exec" => 1, "exit" => 1, "exit!" => 1, "fail" => 1, "fork" => 1, "format" => 1, "freeze" => 1, "getc" => 1, "gets" => 1, "global_variables" => 1, "gsub" => 1, "gsub!" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "iterator?" => 1, "lambda" => 1, "load" => 1, "local_variables" => 1, "loop" => 1, "method_defined?" => 1, "method_missing" => 1, "module_eval" => 1, "name" => 1, "open" => 1, "p" => 1, "print" => 1, "printf" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "proc" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "putc" => 1, "puts" => 1, "raise" => 1, "rand" => 1, "readline" => 1, "readlines" => 1, "require" => 1, "scan" => 1, "select" => 1, "set_trace_func" => 1, "sleep" => 1, "split" => 1, "sprintf" => 1, "srand" => 1, "sub" => 1, "sub!" => 1, "syscall" => 1, "system" => 1, "test" => 1, "throw" => 1, "to_s" => 1, "trace_var" => 1, "trap" => 1, "untrace_var" => 1, "warn" => 1, },
    "Marshal" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "dump" => 1, "freeze" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "load" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "restore" => 1, "to_s" => 1, },
    "MatchData" => {"allocate" => 1, "superclass" => 1, },
    "Math" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "acos" => 1, "acosh" => 1, "ancestors" => 1, "asin" => 1, "asinh" => 1, "atan" => 1, "atan2" => 1, "atanh" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "cos" => 1, "cosh" => 1, "erf" => 1, "erfc" => 1, "exp" => 1, "freeze" => 1, "frexp" => 1, "hypot" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "ldexp" => 1, "log" => 1, "log10" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "sin" => 1, "sinh" => 1, "sqrt" => 1, "tan" => 1, "tanh" => 1, "to_s" => 1, },
    "Method" => {"allocate" => 1, "superclass" => 1, },
    "Module" => {"allocate" => 1, "constants" => 1, "nesting" => 1, "new" => 1, "superclass" => 1, },
    "NilClass" => {"allocate" => 1, "superclass" => 1, },
    "Numeric" => {"allocate" => 1, "new" => 1, "superclass" => 1, },
    "Object" => {"allocate" => 1, "new" => 1, "superclass" => 1, },
    "ObjectSpace" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "_id2ref" => 1, "add_finalizer" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "call_finalizer" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "define_finalizer" => 1, "each_object" => 1, "finalizers" => 1, "freeze" => 1, "garbage_collect" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "remove_finalizer" => 1, "to_s" => 1, "undefine_finalizer" => 1, },
    "Proc" => {"allocate" => 1, "new" => 1, "superclass" => 1, },
    "Process" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "abort" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "detach" => 1, "egid" => 1, "egid=" => 1, "euid" => 1, "euid=" => 1, "exit" => 1, "exit!" => 1, "fork" => 1, "freeze" => 1, "getpgid" => 1, "getpgrp" => 1, "getpriority" => 1, "getrlimit" => 1, "gid" => 1, "gid=" => 1, "groups" => 1, "groups=" => 1, "include?" => 1, "included_modules" => 1, "initgroups" => 1, "instance_method" => 1, "instance_methods" => 1, "kill" => 1, "maxgroups" => 1, "maxgroups=" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "pid" => 1, "ppid" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "setpgid" => 1, "setpgrp" => 1, "setpriority" => 1, "setrlimit" => 1, "setsid" => 1, "times" => 1, "to_s" => 1, "uid" => 1, "uid=" => 1, "wait" => 1, "wait2" => 1, "waitall" => 1, "waitpid" => 1, "waitpid2" => 1, },
    "Process::GID" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "change_privilege" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "eid" => 1, "eid=" => 1, "freeze" => 1, "grant_privilege" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "re_exchange" => 1, "re_exchangeable?" => 1, "rid" => 1, "sid_available?" => 1, "switch" => 1, "to_s" => 1, },
    "Process::Status" => {"allocate" => 1, "superclass" => 1, },
    "Process::Sys" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "freeze" => 1, "getegid" => 1, "geteuid" => 1, "getgid" => 1, "getuid" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "issetugid" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "setegid" => 1, "seteuid" => 1, "setgid" => 1, "setregid" => 1, "setresgid" => 1, "setresuid" => 1, "setreuid" => 1, "setrgid" => 1, "setruid" => 1, "setuid" => 1, "to_s" => 1, },
    "Process::UID" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "change_privilege" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "eid" => 1, "eid=" => 1, "freeze" => 1, "grant_privilege" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "re_exchange" => 1, "re_exchangeable?" => 1, "rid" => 1, "sid_available?" => 1, "switch" => 1, "to_s" => 1, },
    "Range" => {"allocate" => 1, "new" => 1, "superclass" => 1, },
    "Regexp" => {"allocate" => 1, "compile" => 1, "escape" => 1, "last_match" => 1, "new" => 1, "quote" => 1, "superclass" => 1, "union" => 1, },
    "Signal" => {"<" => 1, "<=" => 1, "<=>" => 1, "==" => 1, "===" => 1, ">" => 1, ">=" => 1, "ancestors" => 1, "autoload" => 1, "autoload?" => 1, "class_eval" => 1, "class_variable_defined?" => 1, "class_variables" => 1, "const_defined?" => 1, "const_get" => 1, "const_missing" => 1, "const_set" => 1, "constants" => 1, "freeze" => 1, "include?" => 1, "included_modules" => 1, "instance_method" => 1, "instance_methods" => 1, "list" => 1, "method_defined?" => 1, "module_eval" => 1, "name" => 1, "private_class_method" => 1, "private_instance_methods" => 1, "private_method_defined?" => 1, "protected_instance_methods" => 1, "protected_method_defined?" => 1, "public_class_method" => 1, "public_instance_methods" => 1, "public_method_defined?" => 1, "to_s" => 1, "trap" => 1, },
    "String" => {"allocate" => 1, "new" => 1, "superclass" => 1, },
    "Struct" => {"allocate" => 1, "new" => 1, "superclass" => 1, },
    "Struct::Tms" => {"[]" => 1, "allocate" => 1, "members" => 1, "new" => 1, "superclass" => 1, },
    "Symbol" => {"all_symbols" => 1, "allocate" => 1, "superclass" => 1, },
    "Thread" => {"abort_on_exception" => 1, "abort_on_exception=" => 1, "allocate" => 1, "critical" => 1, "critical=" => 1, "current" => 1, "exit" => 1, "fork" => 1, "kill" => 1, "list" => 1, "main" => 1, "new" => 1, "pass" => 1, "start" => 1, "stop" => 1, "superclass" => 1, },
    "ThreadGroup" => {"allocate" => 1, "new" => 1, "superclass" => 1, },
    "Time" => {"_load" => 1, "allocate" => 1, "at" => 1, "gm" => 1, "local" => 1, "mktime" => 1, "new" => 1, "now" => 1, "superclass" => 1, "times" => 1, "utc" => 1, },
    "TrueClass" => {"allocate" => 1, "superclass" => 1, },
    "UnboundMethod" => {"allocate" => 1, "superclass" => 1, },
  }

  # Returns true if the +method+ on Class or Module +klass+ was defined
  # in MRI.
  def mri?(klass, method)
    mri_instance_method?(klass, method) || mri_class_method?(klass, method)
  end

  def mri_class_method?(klass, method)
    MRI_CLASS_METHODS.has_key?(klass) &&
      MRI_CLASS_METHODS[klass].has_key?(method)
  end

  def mri_instance_method?(klass, method)
    MRI_INSTANCE_METHODS.has_key?(klass) &&
      MRI_INSTANCE_METHODS[klass].has_key?(method)
  end

  # ############################################################
  # Methods below this marker were only used to generate the data
  # ############################################################

  # Return an array of the class and modules names defined in this instance
  # of MRI.
  def self.class_and_module_names
    # Filter out classes we define for now
    filtered = Module.constants.reject { |x| x =~ /#{self.class.name}|MRIInfo/ }
    filtered.sort!
  end

  # Given a class +klass+, return a hash table that has an entry for each
  # of the instance methods in +klass+.  Returns the hash.
  def self.collect_instance_methods_for(klass, methods = Hash.new)
    klass.instance_methods(false).inject(methods) { |h,meth| h[meth] = 1; h}
    methods
  end

  # Given a class +klass+, return a hash table that has an entry for each
  # of the class methods in +klass+.  Returns the hash.
  def self.collect_class_methods_for(klass, methods = Hash.new)
    klass_methods = class << klass;
                      instance_methods(false);
                    end
    klass_methods.inject(methods) { |h,meth| h[meth] = 1; h}
    methods
  end

  # Collect all class methods into a hash and return it
  def self.collect_class_methods(klasses)
    result = Hash.new
    klasses.each do |klass|
      result[klass.name] = self.collect_class_methods_for klass
    end
    result
  end

  # Collect all instance methods into a hash and return it
  def self.collect_instance_methods(klasses)
    result = Hash.new
    klasses.each do |klass|
      result[klass.name] = self.collect_instance_methods_for klass
    end
    result
  end

  # Print the hashes sorted in a manner easy to cut-n-paste into ruby code.
  def self.print_hash_of_hashes(h)
    printf "{"
    h.keys.sort.each do |cname|
      printf "\"%s\" => {", cname
      mhash = h[cname]
      mhash.keys.sort.each { |mname| printf "\"%s\" => 1, ", mname}
      printf "},\n"
    end
    printf "}\n"
  end
end

#MRIInfo.print_hash_of_hashes MRIInfo.collect_class_methods(MRIInfo::CORE)
#MRIInfo.print_hash_of_hashes MRIInfo.collect_instance_methods(MRIInfo::CORE)
