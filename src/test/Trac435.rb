
class String
  @civ_435 = 'trac435'

  def self.get435
    @civ_435
  end
end

unless String.get435 == 'trac435' ; raise 'err' ; end
true
