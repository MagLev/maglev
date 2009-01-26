require 'sequel'

module Sequel
  class Model
    # Force join table generation
    #
    # Usage:
    #   User.create_join(Article, 'articles_users')
    #   # or let the method figure out the correct join table using sequel
    #   # conventions.
    #   User.create_join(Article)
    def self.create_join(to, name = nil)
      from = self
      name ||= [table_name.to_s, to.table_name.to_s].sort.join('_')
      from_key = "#{from.table_name.to_s.singularize}_id"
      to_key = "#{to.table_name.to_s.singularize}_id"

      db.create_table! name do
        primary_key :id
        foreign_key from_key, :class => from
        foreign_key to_key, :class => to
      end
    end
  end
end
