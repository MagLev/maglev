module Ramaze
  module Helper
    module SimpleCaptcha
      def simple_captcha
        question, answer = generate_captcha
        session[:CAPTCHA] = {
          :question => question, :answer => answer.to_s
        }
        question
      end

      def check_captcha(answer)
        if captcha = session[:CAPTCHA]
          should = captcha[:answer].to_s
          should == answer.to_s.strip
        end
      end

      def generate_captcha
        n = [5, 10, 15, 20]
        ns = Array.new(2){ n.sort_by{rand}.first }.sort
        op = rand > 0.42 ? [ns[0], :+, ns[1]] : [ns[1], :-, ns[0]]

        question = op.join(' ')
        answer = op[0].send(op[1], op[2])

        [question, answer]
      end
    end
  end
end
