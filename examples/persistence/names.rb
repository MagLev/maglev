# -*- coding: utf-8 -*-
class RandomNameGenerator

  # Top 100 female names in SSN db for birth year 1962 (most popular to least popular)
  FEMALE_NAMES = [
    'Lisa', 'Mary', 'Susan', 'Karen', 'Linda', 'Patricia', 'Donna',
    'Cynthia', 'Deborah', 'Sandra', 'Lori', 'Pamela', 'Brenda', 'Barbara',
    'Debra', 'Kimberly', 'Sharon', 'Teresa', 'Nancy', 'Elizabeth', 'Laura',
    'Julie', 'Tammy', 'Cheryl', 'Robin', 'Diane', 'Kathy', 'Carol',
    'Kathleen', 'Denise', 'Michelle', 'Debbie', 'Tina', 'Cindy', 'Janet',
    'Angela', 'Kim', 'Kelly', 'Christine', 'Theresa', 'Jennifer', 'Laurie',
    'Margaret', 'Jacqueline', 'Rhonda', 'Carolyn', 'Rebecca', 'Sherry',
    'Maria', 'Dawn', 'Connie', 'Sheila', 'Catherine', 'Terri', 'Tracy',
    'Ann', 'Diana', 'Janice', 'Michele', 'Paula', 'Amy', 'Cathy', 'Judy',
    'Wendy', 'Suzanne', 'Beth', 'Valerie', 'Melissa', 'Stephanie',
    'Beverly', 'Lynn', 'Jill', 'Jane', 'Joyce', 'Annette', 'Renee',
    'Wanda', 'Bonnie', 'Carla', 'Leslie', 'Anita', 'Sherri', 'Anne',
    'Katherine', 'Vicki', 'Judith', 'Martha', 'Kathryn', 'Darlene',
    'Gloria', 'Regina', 'Penny', 'Peggy', 'Joan', 'Jean', 'Shirley',
    'Betty', 'Sarah', 'Andrea', 'Anna' ]

  # Top 100 male names in SSN db for birth year 1962 (most popular to least popular)
  MALE_NAMES = [
    'Michael', 'David', 'John', 'James', 'Robert', 'Mark', 'William',
    'Richard', 'Thomas', 'Jeffrey', 'Scott', 'Steven', 'Joseph', 'Kevin',
    'Charles', 'Timothy', 'Daniel', 'Kenneth', 'Paul', 'Brian', 'Gregory',
    'Gary', 'Donald', 'Anthony', 'Ronald', 'Christopher', 'Douglas',
    'Stephen', 'Edward', 'Todd', 'Larry', 'George', 'Eric', 'Terry',
    'Randy', 'Keith', 'Dennis', 'Jerry', 'Mike', 'Patrick', 'Bruce',
    'Frank', 'Peter', 'Jeff', 'Jeffery', 'Steve', 'Raymond', 'Roger',
    'Andrew', 'Ricky', 'Craig', 'Tony', 'Alan', 'Danny', 'Glenn',
    'Matthew', 'Russell', 'Carl', 'Chris', 'Joe', 'Barry', 'Dale', 'Wayne',
    'Gerald', 'Randall', 'Vincent', 'Troy', 'Phillip', 'Johnny',
    'Lawrence', 'Tim', 'Billy', 'Curtis', 'Martin', 'Jimmy', 'Rodney',
    'Greg', 'Bobby', 'Walter', 'Jay', 'Philip', 'Bryan', 'Darryl', 'Jon',
    'Jim', 'Dean', 'Bradley', 'Samuel', 'Willie', 'Roy', 'Arthur', 'Henry',
    'Allen', 'Jonathan', 'Harold', 'Victor', 'Ralph', 'Jack', 'Tom',
    'Ronnie' ]

  # 100 Most Common U.S. Surnames
  SURNAMES = [
    'Smith', 'Johnson', 'Williams', 'Jones', 'Brown', 'Davis', 'Miller',
    'Wilson', 'Moore', 'Taylor', 'Anderson', 'Thomas', 'Jackson', 'White',
    'Harris', 'Martin', 'Thompson', 'Garcia', 'Martinez', 'Robinson',
    'Clark', 'Rodriguez', 'Lewis', 'Lee', 'Walker', 'Hall', 'Allen',
    'Young', 'Hernandez', 'King', 'Wright', 'Lopez', 'Hill', 'Scott',
    'Green', 'Adams', 'Baker', 'Gonzalez', 'Nelson', 'Carter', 'Mitchell',
    'Perez', 'Roberts', 'Turner', 'Phillips', 'Campbell', 'Parker',
    'Evans', 'Edwards', 'Collins', 'Stewart', 'Sanchez', 'Morris',
    'Rogers', 'Reed', 'Cook', 'Morgan', 'Bell', 'Murphy', 'Bailey',
    'Rivera', 'Cooper', 'Richardson', 'Cox', 'Howard', 'Ward', 'Torres',
    'Peterson', 'Gray', 'Ramirez', 'James', 'Watson', 'Brooks', 'Kelly',
    'Sanders', 'Price', 'Bennett', 'Wood', 'Barnes', 'Ross', 'Henderson',
    'Coleman', 'Jenkins', 'Perry', 'Powell', 'Long', 'Patterson', 'Hughes',
    'Flores', 'Washington', 'Butler', 'Simmons', 'Foster', 'Gonzales',
    'Bryant', 'Alexander', 'Russell', 'Griffin', 'Diaz', 'Hayes', ]

  # Return an array of [first_name, sur_name] for the given gender.  If
  # gender is neither :male nor :female, the gender will be randomly
  # selected.
  # Returns an array of [first_name, surname]
  def self.name_for(gender=nil)
    first_names = case gender
                  when :male
                    MALE_NAMES
                  when :female
                    FEMALE_NAMES
                  else
                    rand(2) == 0 ? MALE_NAMES : FEMALE_NAMES
                  end
    [ first_names.at(rand(100)), SURNAMES.at(rand(100)) ]
  end
end
