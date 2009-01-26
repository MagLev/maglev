#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  # The purpose of this class is to provide an easy way of setting/retrieving
  # from the current flash.
  #
  # Flash is a way to keep a temporary pairs of keys and values for the duration
  # of two requests, the current and following.
  #
  # Very vague Example:
  #
  # On the first request, for example on registering:
  #
  #   flash[:error] = "You should reconsider your username, it's taken already"
  #   redirect R(self, :register)
  #
  # This is the request from the redirect:
  #
  #   do_stuff if flash[:error]
  #
  # On the request after this, flash[:error] is gone.

  module Helper::Flash
    trait :flashbox => "<div class='flash' id='flash_%key'>%value</div>"
    # answers with Session.current.flash

    def flash
      Session.current.flash
    end

    # Use in your template to display all flash messages that may be stored.
    # For example, given you stored:
    #
    #   flash # => { :error => 'Pleae enter your name'
    #                :info => 'Do you see the fnords?' }
    #
    # Then a flashbox would display:
    #
    #   <div class='flash' id='flash_error'>Please enter your name</div>
    #   <div class='flash' id='flash_info'>Do you see the fnords?</div>
    #
    #
    # This can be customized permanently in your controller:
    #
    #   class MainController < Ramaze::Controller
    #     trait :flashbox => "<div class='flash_%key'>%value</div>"
    #
    #     def show_flashbox
    #       flashbox
    #     end
    #   end
    #
    # or on a per-usage basis in your action:
    #
    #   class MainController < Ramaze::Controller
    #     def show_flashbox
    #       flashbox("<div class='flash_%key'>%value</div>")
    #     end
    #   end
    #
    # Where any occurrence of %key and %value will be replaced by the actual
    # contents of each element of flash

    def flashbox(tag = ancestral_trait[:flashbox])
      flash.map{|key, *values|
        values.flatten.map do |value|
          tag.gsub(/%key/, key.to_s).gsub(/%value/, value.to_s)
        end
      }.flatten.join("\n")
    end
  end
end
