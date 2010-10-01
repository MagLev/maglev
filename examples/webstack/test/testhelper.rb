module TestHelper

  def create_and_save(user, pw)
    u = User.signup(user, pw)
    u.save
    u
  end

  def ensure_user_exists(name)
    user = User.find_by_name(name)
    unless user
      user = User.new name, 'pw'
      user.save
    end
    user
  end

  def ensure_no_user_named(name)
    user = User.find_by_name(name)
    user.delete if user
  end

  module_function :create_and_save, :ensure_no_user_named, :ensure_user_exists
end
