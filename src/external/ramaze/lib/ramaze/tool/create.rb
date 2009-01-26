#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'ramaze/tool/project_creator'

module Ramaze
  module Tool

    # Create is a simple class used to create new projects based on the proto
    # directory.
    #
    # It is primarly used for this command:
    #
    #   ramaze --create project
    #
    # where project is the directory you want the content put into.

    class Create

      # Default options passed to Create::create
      #   :proto  is the directory to duplicate
      #   :amend  no files may be overwritten but missing files will be added
      #   :force  will overwrite existing files
      #   :layout copy one subdirectory in +proto+

      DEFAULT = {
        :proto => File.join(::Ramaze::BASEDIR, 'proto'),
        :amend => false,
        :force => false,
        :layout => '/',
      }

      # Using ProjectCreator to copy all files and directories from lib/proto
      # to another location.
      # +options+ are described in the DEFAULT constant and should be:
      #   :force  => (true|false|nil)
      #   :amend  => (true|false|nil)
      #   :layout => (String|nil)
      #   :proto  => String

      def self.create(project, options = {})
        options = DEFAULT.merge(options)
        creator = ProjectCreator.new(project, options)
        creator.create
      end
    end
  end
end
