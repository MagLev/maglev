#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'ramaze/template/ezamar/engine'


module Ezamar

  # A transformer for <render /> tags.
  #
  # Setup:
  #
  #     pipeline = Ramaze::Template::Ezamar::TRANSFORM_PIPELINE
  #     pipeline.put_after(::Ezamar::Element, ::Ezamar::RenderPartial)
  #     pipline.uniq!
  #
  # See /examples/basic/partial.rb for usage.

  class RenderPartial
    extend Ramaze::Helper::Partial

    # Renders <render src="/path" [optional="option", ...]> in place.
    #
    # Other options than `src` will be transformed to session parameters for the
    # rendered action to use.

    def self.transform(template)
      template.gsub!(/<render (.*?) \/>/) do |m|
        args = Hash[*$1.scan(/(\S+)=["'](.*?)["']/).flatten]
        if src = args.delete('src')
          render_partial(src, args)
        end
      end

      template
    end

  end
end
