require 'ramaze/gestalt'

# job = Job[4]
# puts CGI::pretty(FormField.new(job, :contract, show_errors = true, Job::CONTRACTS).to_s)

class FormField
  attr_accessor :this, :field, :show_errors, :hint, :db_type, :id, :value

  # show_errors may be a number indicating how many errors it should display at
  # maximum, 0 displays none, -1 all

  def initialize(this, field, show_errors = true, hint = nil)
    @this, @field, @show_errors, @hint =
      this, field.to_sym, show_errors, hint
    @db_type = db_type
    @id = "form_#{@field}"
    @value = this.send(@field)
  end

  def db_type
    if field_schema = this.class.db_schema[field]
      field_schema[:db_type]
    else
      nil
    end
  end

  def to_s
    unless label = @this.class::FORM_LABEL[field]
      raise("No FORM_LABEL for %p on %p" % [field, @this.class])
    end

    label += ':'
    id = @id
    _self = self

    Ramaze::Gestalt.build{
      div(:class => :pair){
        label(:for => id){ label }
        _self.build_tag(self)
        _self.build_errors(self)
      }
    }
  end

  def password(gestalt)
    gestalt.input(@args.merge :type => :password, :value => value)
  end

  def checkbox(gestalt)
    @args[:checked] = :checked if value
    gestalt.input(@args.merge :type => :checkbox)
  end

  def select(gestalt)
    gestalt.select @args do
      v = value.to_s
      hint.each do |h|
        h = h.to_s
        if h == v
          gestalt.option(:value => h, :selected => :selected){ h }
        else
          gestalt.option(:value => h){ h }
        end
      end
    end
  end

  def input(gestalt)
    gestalt.input(@args.merge :type => :text, :value => value)
  end

  def file(gestalt)
    gestalt.input(@args.merge :type => :file)
  end

  def textarea(gestalt)
    gestalt.textarea(@args){ value.to_s }
  end

  def build_tag(gestalt)
    @args = {:id => id, :name => field}

    case hint
    when :password
      password(gestalt)
    when :boolean, :checkbox
      checkbox(gestalt)
    when :textarea
      textarea(gestalt)
    when :file
      file(gestalt)
    when Array, Range
      select(gestalt)
    else
      case db_type
      when 'varchar', 'integer'
        input(gestalt)
      when 'boolean'
        checkbox(gestalt)
      when 'string'
        textarea(gestalt)
      else
        raise "Unsupported type: (#{db_type || hint} : #{field})"
      end
    end
  end

  def build_errors(gestalt)
    each_error do |error|
      gestalt.span(:class => :error){ error }
    end
  end

  # TODO: suggest 'validated?' feature to sequel
  def each_error(&block)
    return unless show_errors

    this.validate if this.errors.empty?
    this.errors[field].first(show_errors).each(&block)
  end

  module Model
    def form_field(field, hint = nil)
      show_errors = Ramaze::Request.current.post? ? 1 : 0
      FormField.new(self, field, show_errors, hint).to_s
    end
  end
end
