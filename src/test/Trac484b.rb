# Variation on const_missing error seen with sqlite3 gem

module SQLite3
  class Database
    class FunctionProxy
    end

    def create_aggregate_handler
      proxy = Class.new do
        def initialize
          @fp = FunctionProxy.new # failure to resolve FunctionProxy 
        end
      end
      define_aggregator(proxy.new) 
    end
  
    def define_aggregator(p)
      $px = p.instance_variable_get(:'@fp')
    end
  end
end

SQLite3::Database.new.create_aggregate_handler

unless $px.class.equal?(SQLite3::Database::FunctionProxy); raise 'fail' ;end
true
