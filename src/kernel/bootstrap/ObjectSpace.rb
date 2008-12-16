module ObjectSpace

  def _id2ref(object_id)
    raise NotImplementedError, ' _id2ref not implemented'
  end


  def define_finalizer(obj)
    raise NotImplementedError, ' define_finalizer not implemented'
  end

  def each_object(class_or_module, &block)
    raise NotImplementedError, ' each_object not implemented'
  end

  def garbage_collect()
    # has no effect in Maglev
    return nil
  end

  def undefine_finalizer()
    raise NotImplementedError, ' undefine_finalizer not implemented'
  end

end
