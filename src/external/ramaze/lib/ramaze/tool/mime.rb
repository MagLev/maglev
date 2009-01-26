#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module Tool

    # Responsible for lookup of MIME info for filetypes based on extension.

    module MIME

      # the mime_types.yaml as full path, we use a copy of mongrels.
      trait :types => YAML.load_file(File.join(BASEDIR, 'ramaze/tool/mime_types.yaml'))

      class << self

        # Get MIME-type for the given filename based on extension.
        # Answers with an empty string if none is found.
        def type_for(file)
          ext = File.extname(file)
          trait[:types][ext].to_s
        end

        def ext_for(mime)
          exts = []

          trait[:types].each do |ext, mime_type|
            exts << ext if mime == mime_type
          end

          exts.sort_by{|e| e.size }.first
        end
      end
    end
  end
end
