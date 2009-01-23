
require 'rbyaml/yaml'
require 'rbyaml/stream'
require 'rbyaml/dumper'

module RbYAML
  def self.dump(obj, io = nil, opts={})
    _dump(obj,io,Dumper,opts)
  end

  def self.load( io )
    _load(io)
  end

  def self.load_file( filepath )
    File.open( filepath ) do |f|
      load( f )
    end
  end

  # this operation does not make sense in RbYAML (right now)
  def self.parse( io )
    #    yp = @@parser.new( :Model => :Generic ).load( io )
  end

  # this operation does not make sense in RbYAML (right now)
  def self.parse_file( filepath )
    #    File.open( filepath ) do |f|
    #      parse( f )
    #    end
  end

  def self.each_document( io, &block )
    _load_all(io,&block)
  end

  def self.load_documents( io, &doc_proc )
    each_document( io, &doc_proc )
  end

  # this operation does not make sense in RbYAML (right now)
  def self.each_node( io, &doc_proc )
    #    yp = @@parser.new( :Model => :Generic ).load_documents( io, &doc_proc )
  end

  # this operation does not make sense in RbYAML (right now)
  def self.parse_documents( io, &doc_proc )
    #    YAML.each_node( io, &doc_proc )
  end
  
  def self.load_stream( io )
    d = nil
    load_documents(io) { |doc|
      d = Stream.new( nil ) if not d
      d.add( doc ) 
    }
    d
  end

  def self.dump_stream( *objs )
    d = RbYAML::Stream.new
    objs.each do |doc|
      d.add( doc ) 
    end
    d.emit
  end


  def self.add_builtin_ctor(type_tag, &transfer_proc)
    BaseConstructor::add_constructor("tag:yaml.org,2002:#{ type_tag }",transfer_proc)
  end

  # this operation does not make sense in RbYAML (right now)
  def self.add_domain_type( domain, type_re, &transfer_proc )
    #    @@loader.add_domain_type( domain, type_re, &transfer_proc )
  end

  # this operation does not make sense in RbYAML (right now)
  def self.add_builtin_type( type_re, &transfer_proc )
    #    @@loader.add_builtin_type( type_re, &transfer_proc )
  end

  # this operation does not make sense in RbYAML (right now)
  def self.add_ruby_type( type_tag, &transfer_proc )
    #    @@loader.add_ruby_type( type, &transfer_proc )
  end

  # this operation does not make sense in RbYAML (right now)
  def self.add_private_type( type_re, &transfer_proc )
    #    @@loader.add_private_type( type_re, &transfer_proc )
  end

  def self.detect_implicit( val )
    SimpleDetector.detect(val)
  end

  # this operation does not make sense in RbYAML (right now)
  def self.transfer( type_id, obj )
    #    @@loader.transfer( type_id, obj )
  end

  # this operation does not make sense in RbYAML (right now)
  def self.try_implicit( obj )
    #    YAML.transfer( YAML.detect_implicit( obj ), obj )
  end

  def self.read_type_class( type, obj_class )
    scheme, domain, type, tclass = type.split( ':', 4 )
    tclass.split( "::" ).each { |c| obj_class = obj_class.const_get( c ) } if tclass
    return [ type, obj_class ]
  end

  def self.object_maker( obj_class, val )
    if Hash === val
      o = obj_class.allocate
      val.each_pair { |k,v|
        o.instance_variable_set("@#{k}", v)
      }
      o
    else
      raise YAMLError, "Invalid object explicitly tagged !ruby/Object: " + val.inspect
    end
  end

  # this operation does not make sense in RbYAML (right now)
  def self.quick_emit( oid, opts = {}, &e )
  end

  def self.quick_emit_node( oid, rep, &e )
    e.call(rep)
  end
end

require 'rbyaml/tag'
require 'rbyaml/types'
require 'rbyaml/rubytypes'
