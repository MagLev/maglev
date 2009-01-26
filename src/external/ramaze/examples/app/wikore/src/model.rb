require 'sequel'

begin
  case $wikore_db
  when :memory
    DB = Sequel.sqlite
  else
    DB_FILE = __DIR__('wikore.db')
    DB = Sequel.connect("sqlite://#{DB_FILE}")
  end
rescue NoMethodError
  raise LoadError, 'Install latest Sequel gem'
end

module Model
  PAGE_SCHEMA = lambda{
    primary_key :id
    boolean :active, :default => true
    text    :text
    integer :version
  }

  class Page < Sequel::Model(:page)
    set_schema do
      instance_eval(&PAGE_SCHEMA)
      text :title, :unique => true, :null => false
    end

    def backup
      hash = @values.dup
      hash.delete :id
      OldPage.create(hash)
    end

    def revert
      backup = OldPage[:title => title].values.dup
      backup.delete :id
      delete
      self.class.create(backup)
    end
  end

  class OldPage < Sequel::Model(:old_page)
    set_schema do
      instance_eval(&PAGE_SCHEMA)
      text :title, :unique => false, :null => false
    end
  end

  [Page, OldPage].each do |klass|
    klass.create_table unless klass.table_exists?
  end
end
