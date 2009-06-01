module ObjectSpace
  def _id2ref(object_id)
    raise NotImplementedError, ' _id2ref not implemented'
  end

  def define_finalizer(obj, a_proc=nil)
    raise NotImplementedError, ' define_finalizer not implemented'
  end

  def each_object(class_or_module=nil, &block)
    raise NotImplementedError, ' each_object not implemented'
  end

  def garbage_collect()
    # has no effect in Maglev
    return nil
  end

  def undefine_finalizer(obj)
    raise NotImplementedError, ' undefine_finalizer not implemented'
  end

  module_function :_id2ref, :define_finalizer, :each_object, :garbage_collect, :undefine_finalizer
end
