DateAndTime = __resolve_smalltalk_global(:DateAndTime)
class DateAndTime
  primitive 'as_posix_seconds', 'asPosixSeconds'
  def as_time
    psecs = as_posix_seconds
    secs = psecs.to_i
    usecs = psecs - secs
    Time.at(secs, usecs)
  end
end
