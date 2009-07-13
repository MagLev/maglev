Maglev.persistent do
  # define some methods to test whether they are removed properly
  class C004
    def self.cm_one;   :self_cm_one; end
    def self.cm_two;   :self_cm_two; end
    def self.cm_three; :self_cm_three; end

    def im_one;   :im_one; end
    def im_two;   :im_two; end
    def im_three; :im_three; end
  end
end
Maglev.commit_transaction
