module W
  def what(where=95)
    yield where
  end
end

class C1
  include W
end

class C2 < C1
  def what(where=98)
    super where 
  end
end

o = C2.new
$v454 = 0
o.what { | arg | $v454 = arg }
unless $v454 == 98 ; raise 'error'; end 
true
#################### Trac Info
# ID:         454
# Summary:    Block passing to super problem
# Changetime: 2009-04-22 15:48:37+00:00
###

#  The following shows where a block is not being propagated properly. The attached test case recreates it.
#  
#  ERROR 2023, Error, 'no block was passed'
#  topaz 1> frame 14
#  14 SchemaStatements >> create_table::&      (envId 1) @16 line 5
#      receiver [112172801  MysqlAdapter] aMysqlAdapter
#      table_name [112167937 sz:17  String] schema_migrations
#      options [112167425 sz:6  RubyHash] aRubyHash
#      _block nil
#      create_sql nil
#      table_definition [112166401  TableDefinition] aTableDefinition
#  topaz 1> list
#           def create_table(table_name, options = {})
#             table_definition = TableDefinition.new(self)
#             table_definition.primary_key(options[:primary_key] || Base.get_primary_key(table_name)) unless options[:id] == false
#     
#             yield table_definition
#   * ^16                                                                *******
#     
#             if options[:force] && table_exists?(table_name)
#               drop_table(table_name, options)
#             end
#     
#             create_sql = "CREATE#{' TEMPORARY' if options[:temporary]} TABLE "
#             create_sql << "#{quote_table_name(table_name)} ("
#             create_sql << table_definition.to_sql
#             create_sql << ") #{options[:options]}"
#             execute create_sql
#     
#     # method starts at line 100 of file /Users/lattam/Projects/MagLev/working/maglev_test-save/vendor/rails-save/activerecord/lib/active_record/connection_adapters/abstract/schema_statements.rb 
#  topaz 1> frame 15
#  15 SchemaStatements >> create_table::       (envId 1) @2 line 1
#      receiver [112172801  MysqlAdapter] aMysqlAdapter
#      t1 [112167937 sz:17  String] schema_migrations
#      t2 [112167425 sz:6  RubyHash] aRubyHash
#  topaz 1> list
#     <a Ruby bridge method>
#   * ^2                                                                 *******
#       
#  topaz 1> frame 16
#  16 MysqlAdapter >> create_table::           (envId 1) @8 line 2
#      receiver [112172801  MysqlAdapter] aMysqlAdapter
#      table_name [112167937 sz:17  String] schema_migrations
#      options [112136193 sz:6  RubyHash] aRubyHash
#  topaz 1> list
#           def create_table(table_name, options = {}) #:nodoc:
#             super(table_name, options.reverse_merge(:options => "ENGINE=InnoDB"))
#   *   ^8                                                                          
#     
#     # method starts at line 472 of file /Users/lattam/Projects/MagLev/working/maglev_test-save/vendor/rails-save/activerecord/lib/active_record/connection_adapters/mysql_adapter.rb 
#  topaz 1> frame 17
#  17 MysqlAdapter >> create_table::&          (envId 1) @2 line 1
#      receiver [112172801  MysqlAdapter] aMysqlAdapter
#      t1 [112167937 sz:17  String] schema_migrations
#      t2 [112136193 sz:6  RubyHash] aRubyHash
#      block [112110849 sz:0  ExecBlock] anExecBlock
#  topaz 1> list
#     <a Ruby bridge method>
#   * ^2                                                                 *******
#       
#  topaz 1> frame 18
#  18 SchemaStatements >> initialize_schema_migrations_table (envId 1) @11 line 5
#      receiver [112172801  MysqlAdapter] aMysqlAdapter
#      old_version nil
#      si_table nil
#      sm_table [112167937 sz:17  String] schema_migrations
#  topaz 1> list
#           def initialize_schema_migrations_table
#             sm_table = ActiveRecord::Migrator.schema_migrations_table_name
#     
#             unless tables.detect { |t| t == sm_table }
#               create_table(sm_table, :id => false) do |schema_migrations_table|
#   *  ^11                                                                        
#                 schema_migrations_table.column :version, :string, :null => false
#               end
#               add_index sm_table, :version, :unique => true,
#                 :name => 'unique_schema_migrations'
#     
#               # Backwards-compatibility: if we find schema_info, assume we've
#               # migrated up to that point:
#               si_table = Base.table_name_prefix + 'schema_info' + Base.table_name_suffix
#     
#               if tables.detect { |t| t == si_table }
#     
#                 old_version = select_value("SELECT version FROM #{quote_table_name(si_table)}").to_i
#                 assume_migrated_upto_version(old_version)
#                 drop_table(si_table)
#     
#     # method starts at line 316 of file /Users/lattam/Projects/MagLev/working/maglev_test-save/vendor/rails-save/activerecord/lib/active_record/connection_adapters/abstract/schema_statements.rb 
#  topaz 1> 
#  
#  