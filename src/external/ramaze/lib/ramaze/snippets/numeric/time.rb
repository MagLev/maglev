module Ramaze
  module CoreExtensions

    # Extensions for Numeric

    module Numeric
      def seconds
        self
      end
      alias second seconds

      # 60 seconds in a minute
      def minutes
        self * 60
      end
      alias minute minutes

      # 60 minutes in an hour
      def hours
        self * 3600
      end
      alias hour hours

      # 24 hours in a day
      def days
        self * 86400
      end
      alias day days

      # 7 days in a week
      def weeks
        self * 604800
      end
      alias week weeks

      # 30 days in a month
      def months
        self * 2592000
      end
      alias month months

      # 365.25 days in a year
      def years
        self * 31557600
      end
      alias year years

      # Time in the past, i.e. 3.days.ago
      def ago t = Time.now
        t - self
      end
      alias before ago

      # Time in the future, i.e. 3.days.from_now
      def from_now t = Time.now
        t + self
      end
      alias since from_now

    end

  end
end
