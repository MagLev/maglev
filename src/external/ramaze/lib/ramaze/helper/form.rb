module Ramaze
  module Helper
    module Form
      # Pass it an object for your ORM and options for the <form> tag
      # Usage:
      #   form_for(User, :action => '/create')
      #   form_for(Tag, :action => '/find', :method => 'GET')
      def form_for(object, options = {})
        Ramaze::Form.pick(object, options)
      end
    end
  end

  class Form
    attr_accessor :object, :options

    YEARS, MONTHS, DAYS, HOURS, MINUTES, SECONDS =
      (1900..2100), (1..12), (1..31), (0..23), (0..59), (0..59)

    DATE_GENERIC = [
      [ :day,   DAYS ],
      [ :month, MONTHS ],
      [ :year,  YEARS ] ]

    TIME_GENERIC = [
      [ :day,   DAYS ],
      [ :month, MONTHS ],
      [ :year,  YEARS ],
      [ :hour,  HOURS ],
      [ :min,   MINUTES ],
      [ :sec,   SECONDS ] ]

    # TODO:
    #   How _elegant_ ...
    #   Tries to find the right module for extending the Form instance.
    #   It's problematic since the boundaries of what an model instance or model
    #   class looks like is very fuzzy, also a problem is that the ORM may not be
    #   available/required.
    #
    #   Maybe we can abstract that a bit by going through an array of procs for
    #   testing?
    def self.pick(object, options = {})
      if defined?(Sequel::Model)
        if object.is_a?(Sequel::Model)
          options[:layer] ||= Layer::Sequel
          InstanceForm.new(object, options)
        elsif object.ancestors.include?(Sequel::Model)
          options[:layer] ||= Layer::Sequel
          ClassForm.new(object, options)
        end
      else
        raise "Unknown ORM for: %p" % object
      end
    end

    # Create new instance of Form plus the layer for the ORM
    def initialize(object, options = {})
      @object, @options = object, options
      if layer = options.delete(:layer)
        extend layer
      end
    end

    # Generate and return the final form
    def to_s
      out = "<form #{form_attributes}>"
      out << "<fieldset>"
      out << generate
      out << "</fieldset>"
      out << '<input type="submit" />'
      out << '<input type="reset" />'
      out << "</form>"
    end

    # Decide on the strucuture of the tag based on the hash
    def field_for(hash)
      return if hash[:primary_key]
      args = args_for(hash)

      inner =
        case type = hash[:type]
        when :integer
          field_integer(*args)
        when :boolean
          field_boolean(*args)
        when :text
          field_textarea(*args)
        when :varchar
          field_input(*args)
        when :date
          field_date(*args)
        when :time
          field_time(*args)
        else
          Log.warn "Unknown field: %p" % hash
          field_input(*args)
        end

      "<label>#{args.first}: </label>\n#{inner}"
    end

    private

    # inject to attributes for the <form>
    def form_attributes
      options.inject([]){|s,(k,v)| s << "#{k}='#{v}'" }.join(' ')
    end

    # Start tag with name and attributes
    def start_tag(name, hash)
      hash.inject("<#{name}"){|s,(k,v)| s << " #{k}='#{v}'" }
    end

    # Make a closed tag with name and attributes
    def closed_tag(name, hash)
      start_tag(name, hash) << ' />'
    end

    # Textarea with attributes from hash and the value from @object
    def textarea(value, hash = {})
      start_tag(:textarea, hash) << ">#{value}</textarea>"
    end

    # <input> with optional attributes from hash
    def input(hash = {})
      closed_tag(:input, hash)
    end

    # <input type="checkbox" with optional attributes from hash.
    def checkbox(hash = {})
      hash[:type] = :checkbox
      input(hash)
    end

    # <option value="value"> with optional attributes from hash
    def option(value, hash = {})
      start_tag(:option, hash) << ">#{value}</option>"
    end

    # Yield method names and values for the Date instance
    def field_date_generic
      DATE_GENERIC.map{|(sel, range)|
        yield(sel, range).join
      }.join("\n")
    end

    # Yield method names and values for the Time/DateTime instance
    def field_time_generic
      TIME_GENERIC.map{|(sel, range)|
        yield(sel, range).join
      }.join("\n")
    end

    # Here go all the layers that are extended for specific ORMs
    module Layer
      # Layer for Sequel, only generate needs to be implemented, may change in
      # future if we abstract more for different ORMs
      module Sequel
        # A bit nasty, get the @columns of the object and generate its
        # field_for
        def generate
          columns = object_class.schema.instance_variable_get('@columns')
          columns.map{|hash| field_for(hash) }.flatten.join("<br />\n")
        end
      end
    end
  end

  # Form for the model class itself, very similar to an empty instance.
  class ClassForm < Form
    # <input name="name" />
    def field_input(name)
      input :name => name
    end

    # <textarea name="name"></textarea>
    def field_textarea(name)
      textarea '', :name => name
    end

    # <input name="name" />
    def field_integer(name)
      input :name => name
    end

    # <input type="checkbox" name="name" />
    def field_boolean(name)
      checkbox :name => name
    end

    # <select> with lots of <option>s
    def field_date(name)
      field_date_generic{|sel, range|
        [ "<select name='#{name}[#{sel}]'>",
          range.map{|d| option(d, :value => d) },
          "</select>" ]
      }
    end

    # <select> with lots of <option>s
    def field_time(name)
      field_time_generic{|sel, range|
        [ "<select name='#{name}[#{sel}]'>",
          range.map{|d| option(d, :value => d) },
          "</select>" ]
      }
    end

    # picks the :name
    def args_for(hash)
      [ hash[:name] ]
    end

    # Should be that way, at least for Sequel
    def object_class
      @object
    end
  end

  # Form for instances of the model class
  class InstanceForm < Form
    # returns <input type='text' name='name' value='value' />
    def field_input(name, value)
      "<input type='text' name='#{name}' value='#{value}'/>"
    end

    # returns <textarea name='name'>#{value}</textarea>

    def field_textarea(name, value)
      "<textarea name='#{name}'>#{value}</textarea>"
    end

    # returns <input type="text" name="name" value="value" />

    def field_integer(name, value)
      field_input(name, value)
    end

    # <input type="checkbox" ...
    def field_boolean(name, value)
      if value
        checkbox :name => name, :value => value, :checked => :checked
      else
        checkbox :name => name, :value => value
      end
    end

    def field_date(name, value)
      field_date_generic do |sel, range|
        [ "<select name='#{name}[#{sel}]'>",
          option_range_selected(range, value.send(sel)),
          "</select>" ]
      end
    end

    def field_time(name, value)
      field_time_generic do |sel, range|
        [ "<select name='#{name}[#{sel}]'>",
          option_range_selected(range, value.send(sel)),
          "</select>" ]
      end
    end

    def option_range_selected(range, value)
      range.map do |r|
        if r == value
          option(r, :value => r, :selected => :selected)
        else
          option(r, :value => r)
        end
      end
    end

    def args_for(hash)
      name = hash[:name]
      [ name, @object.send(name) ]
    end

    # Class for @object, atm Sequel specific?
    def object_class
      @object.class
    end
  end
end
