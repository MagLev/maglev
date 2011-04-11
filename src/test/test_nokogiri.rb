# NOTE, run with
#   LD_LIBRARY_PATH=/export/iceland1/users/otisa/linuxSrc/solaris_xml2/libxml2-2.7.7/.libs:/lib:/usr/lib
# setenv LD_LIBRARY_PATH '/export/iceland1/users/otisa/linuxSrc/solaris_xml2/libxml2-2.7.7/.libs:/lib:/usr/lib'
#  libxml2 version installed on  energy in /usr/lib/amd64 is  2.6.23 

Dir.chdir('/export/iceland1/users/otisa/Svn/st64_30alt/git/cExtensions/nokogiri')
lp = $LOAD_PATH 
lp.insert(0, 'lib/' )
load 'lib/nokogiri.rb'
require 'rubygems'

lp << 'test/'

if true
 # running ok
 load 'test/test_nokogiri.rb'
 load 'test/html/test_document.rb'
 load 'test/html/test_element_description.rb'
 load 'test/html/test_named_characters.rb'
 load 'test/html/sax/test_parser_context.rb'
 load 'test/test_convert_xpath.rb'
 load 'test/test_soap4r_sax.rb'
 load 'test/xslt/test_custom_functions.rb'
 load 'test/test_css_cache.rb'
 load 'test/test_memory_leak.rb'
 load 'test/xslt/test_exception_handling.rb'
 load 'test/css/test_xpath_visitor.rb'
 load 'test/css/test_parser.rb'
 load 'test/css/test_nthiness.rb'
 load 'test/css/test_tokenizer.rb'
 load 'test/xml/test_builder.rb'
 load 'test/xml/test_entity_reference.rb'
 load 'test/xml/test_element_decl.rb'
 load 'test/xml/test_dtd.rb'
 load 'test/xml/test_syntax_error.rb'
 load 'test/xml/test_xpath.rb'
 load 'test/xml/test_node_attributes.rb'
 load 'test/xml/test_parse_options.rb'
 load 'test/xml/test_entity_decl.rb'
 load 'test/xml/test_schema.rb'
 load 'test/xml/test_namespace.rb'
 load 'test/xml/test_attribute_decl.rb'
 load 'test/xml/test_attr.rb'
 load 'test/xml/node/test_subclass.rb'
 load 'test/xml/node/test_save_options.rb'
 load 'test/xml/test_relax_ng.rb'  # ok with 2.7.7 lib
 load 'test/xml/test_node.rb' # ok with 2.7.7 lib
 load 'test/html/test_document_fragment.rb'  # ok with 2.7.7 lib
 load 'test/html/test_node.rb'  # ok with 2.7.7 lib
 load 'test/xml/test_cdata.rb'
 load 'test/xml/test_document_fragment.rb'
 load 'test/xml/test_node_set.rb'
 load 'test/xml/test_processing_instruction.rb'
 load 'test/xml/test_unparented_node.rb'
 load 'test/xml/test_comment.rb'
 load 'test/xml/test_element_content.rb'
 load 'test/decorators/test_slop.rb'
 load 'test/xml/sax/test_parser_context.rb' 
 load 'test/xml/sax/test_push_parser.rb'  
 load 'test/xml/test_dtd_encoding.rb'
 load 'test/xml/test_document_encoding.rb'
 load 'test/xml/test_node_encoding.rb'
 load 'test/xml/test_reader_encoding.rb'
 load 'test/html/test_node_encoding.rb'
 load 'test/test_encoding_handler.rb'
 load 'test/xml/test_text.rb' 
 load 'test/xml/sax/test_parser.rb' 
end
if false
 #
 load 'test/test_reader.rb' #  non-NULL dmark needed
 load 'test/xml/test_node_reparenting.rb' # fails
 load 'test/test_xslt_transforms.rb'  # test_z_exslt gets empty string
 load 'test/xml/test_document.rb' # ok up to test_z_encoding EUC-JP
 load 'test/html/sax/test_parser.rb'  # some encoding deviations
 load 'test/html/test_document_encoding.rb'  # fails Shift_JIS
 load 'test/html/test_builder.rb'  # test_z_instance_eval_with_delegation_to_block_context , already has root node
end
