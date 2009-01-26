#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

# an Element is almost like an Controller, however, instead
# of connecting actions to templates it is only used in
# Ramaze::Template::Ezamar and can be used inside the
# templates of the Controller as a simple wrapper.
#
# Example:
#
# Your Element called Page:
#
#   class Page < Ezamar::Element
#     def render
#       %{
#        <html>
#          <h1>
#            #@title
#          </h1>
#          #{content}
#        </html>
#        }
#     end
#   end
#
# and one called SideBar
#
#   class SideBar < Ezamar::Element
#     def render
#       %{
#         <a href="http://something.com">something</a>
#        }
#      end
#    end
#
# and your template (any template for any action):
#
#   <Page title="Test">
#     <SideBar />
#     <p>
#       Hello, World!
#     </p>
#   </Page>
#
# would result in:
#
#   <html>
#     <h1>
#       Test
#     </h1>
#     <p>
#       Hello, World!
#     </p>
#   </html>

class Ezamar::Element
  attr_accessor :content

  include Ramaze::Helper::Methods

  helper :redirect, :link

  # this will be called by #transform, passes along the
  # stuff inside the tags for the element

  def initialize(content)
    @content = content
  end

  # The method that will be called upon to render the things
  # inside the element, you can access #content from here, which
  # contains the contents between the tags.
  #
  # It should answer with a String.

  def render *args
    @content
  end

  class << self
    # transforms all <Element> tags within the string, takes also
    # a binding to be compatible to the transform-pipeline, won't have
    # any use for it though.

    def transform template
      matches = template.scan(/<([A-Z][a-zA-Z0-9]*)(.*?)?>/)

      matches.each do |(klass, params)|
        transformer = (params[-1,1] == '/' ? :without : :with)
        template = send("transform_#{transformer}_content", template, klass)
      end
      template
    end

    # transforms elements like:
    #   <Page> some content </Page>

    def transform_with_content(template, klass)
      template.gsub(/<#{klass}( .*?)?>(.*?)<\/#{klass}>/m) do |m|
        params, content = $1.to_s, $2.to_s
        finish_transform(klass, params, content)
      end
    end

    # transforms elements like:
    #   <Page />

    def transform_without_content(template, klass)
      template.gsub(/<#{klass}( .*?)?\/>/) do |m|
        params = $1.to_s
        finish_transform(klass, params, content = '')
      end
    end

    # find the element, create an instance, pass it the content
    # check if it responds to :render and sets instance-variables
    # that are named after the keys and hold the values of the parameters
    # you passed to the Element
    #
    # Parameters look like:
    #   <Page foo="true"> bar </Page>
    #   <Page foo="true" />

    def finish_transform(klass, params, content)
      scope =
        if Ramaze::Action.current
          Ramaze::Action.current.controller
        else
          self.class
        end
      instance = scope.constant(klass).new(content)

      demunge_passed_variables(params).each do |key, value|
        instance.instance_variable_set("@#{key}", value)
      end

      instance.render
    rescue => ex
      Ramaze::Log.debug(ex.message)
      ''
    end

    # basically processes stuff like
    #   'foo="bar" foobar="baz"'
    # do NOT pass actual objects that cannot be simply read as a string
    # here, the information will be lost.
    #
    # Exceptions are true, false, Integers and Floats. They will appear
    # in their real form (this again is also valid for strings that contain
    # these values in a way that makes Integer/Float possible to parse them)
    #
    # Just remember, walk like a duck, talk like a duck.

    def demunge_passed_variables(template)
      template.scan(/\s?(.*?)=["'](.*?)["']/).inject({}) do |hash, (key, value)|
        value =
          case value
          when 'true'
            true
          when 'false'
            false
          else
            Integer(value) rescue Float(value) rescue value
          end
        hash.merge key => value
      end
    end
  end
end
