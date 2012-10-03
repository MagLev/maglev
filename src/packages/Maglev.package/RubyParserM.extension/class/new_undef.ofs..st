*maglev-ast
new_undef: symNode ofs: srcOfs
  ^ RubyUndefNode _basicNew name: symNode symNodeValue ; position: srcOfs

