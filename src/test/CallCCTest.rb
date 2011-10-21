# -*- coding: utf-8 -*-
# Modeled after Andrzej Filinski's article "Representing
# Monads" at POPL'94, and a Scheme implementation of it.
# http://citeseer.ist.psu.edu/filinski94representing.html
#
# Copyright 2004–2011 Christian Neukirchen
class ShiftReset
  @@metacont = lambda { |x|
    raise RuntimeError, "You forgot the top-level reset..."
  }

  def reset(&block)
    mc = @@metacont
    callcc { |k|
      @@metacont = lambda { |v|
        @@metacont = mc
        k.call v
      }
      x = block.call
      @@metacont.call x
    }
  end

  def shift(&block)
    callcc { |k|
      @@metacont.call block.call(lambda { |*v|
                                   reset { k.call *v } })
    }
  end

  def foo
    1 + shift {|k| k.call(k.call(k.call(7))) }
  end

  def bar
    foo * 2
  end

  def baz
    reset { bar }
  end
end

raise "#callcc doesn't work" unless ShiftReset.new.baz == 70
