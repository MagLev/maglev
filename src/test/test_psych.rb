# Dir.chdir('/export/iceland1/users/otisa/gitPublic/rubyCexts/psych')
lp = $LOAD_PATH 
lp.insert(0, 'lib/' )
load 'lib/psych.rb'
require 'rubygems'
module Kernel
  def require_relative(arg)
    self.require(File.expand_path(arg, './test/psych/'))
  end
end

if false
 # working tests
 load 'test/psych/test_boolean.rb'
 load 'test/psych/test_null.rb'
 load 'test/psych/test_scalar.rb'
 load 'test/psych/test_symbol.rb'
 load 'test/psych/test_document.rb'
 load 'test/psych/test_scalar_scanner.rb'
 load 'test/psych/test_serialize_subclasses.rb'
 load 'test/psych/test_array.rb'
 load 'test/psych/test_omap.rb'
 load 'test/psych/test_object.rb'
 load 'test/psych/test_string.rb'
 load 'test/psych/test_alias_and_anchor.rb'
 load 'test/psych/test_class.rb'
 load 'test/psych/test_set.rb'
 load 'test/psych/test_stream.rb'
 load 'test/psych/test_struct.rb'
 load 'test/psych/test_emitter.rb'
 load 'test/psych/test_exception.rb'
 load 'test/psych/test_hash.rb'
 load 'test/psych/test_json_tree.rb'
 load 'test/psych/test_tree_builder.rb'
 load 'test/psych/visitors/test_emitter.rb'
 load 'test/psych/test_to_yaml_properties.rb'
 load 'test/psych/test_coder.rb'
 load 'test/psych/test_parser.rb'  
 load 'test/psych/test_psych.rb' 
  load 'test/psych/test_yaml.rb' # test_z problems with load_documents, Rational , Complex, Time
end

if false
  load 'test/psych/visitors/test_to_ruby.rb'   # problems with Time, Complex, Rational,  patch to path2class call
  load 'test/psych/visitors/test_yaml_tree.rb'  # problems in Time, Complex
  load 'test/psych/test_date_time.rb'  # Time vs DateTime problems
end

#load 'test/psych/test_deprecated.rb'
#load 'test/psych/test_engine_manager.rb'
#load 'test/psych/test_encoding.rb'   # 1.9 specific

