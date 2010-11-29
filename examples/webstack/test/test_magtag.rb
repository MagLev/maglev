require "minitest/spec"
require "magtag"

MiniTest::Unit.autorun

describe MagTag do
  it 'answers: who shares my interests?'
  it 'answers: what interests do those users have that I might find interesting?'
  it 'answers: what is my immediate and fringe ecosystem?'

  # For the counts, the MySQL solution uses in-memory summary tables that
  # are generated from the data at startup and maintained by the RDBMS
  it 'answers: count of tags'
  it 'answers: count of users'
  it 'answers: count of items'
  it 'answers: count of items tagged with a tag'

  # A tag cloud is a mapping of Tag => Count for a specific type of item.
  # There may be a limit (e.g., top ten, or only tags that are used more
  # than 5 times (i.e., at least 5 users tagged an item with a tag).
  it 'generates a tag cloud for an item (with a limit)'
  it 'generates a tag cloud for a user (with a limit)'

  #########################################
  # FIND ITEMS
  #########################################
  it 'finds items tagged with any of a set of tags'  # :good OR :bad
  it 'finds items tagged with all of a set of tags'  # :good AND :nutritious

  #########################################
  # FIND TAGS
  #########################################

  # Opposite of find related items: Get all tags related to a particular
  # tag, by looking at an item's tags.
  it 'finds related tags via an item'

  it 'finds related items of all of a set of tags'
  it 'finds unrelated items'

  # Get all items tagged with any tag attached to item #6
  # (get all items related to item 6)

  #########################################
  # FIND USERS
  #########################################
  # Find users that linked/tagged an item I tagged/linked (direct)
  #    SELECT user_id FROM UserTagPost WHERE post_id = @my_post_id;
  it 'finds users who share my interests (direct)'

  # Find users that linked/tagged an item I tagged/linked (indirect)
  # 1: Find the tags I use the most (used more times than @radius):
  #   SELECT tag_id
  #   FROM UserTagPost
  #   WHERE user_id = @my_user_id
  #   GROUP BY tag_id
  #   HAVING COUNT(tag_id) >= @radius
  # 2: Find users who match all of my "top tags"
  it 'finds users who share my interests (indirect)'

  it 'finds other interests of users who share my interests'
  it 'finds users that tagged items I tagged'

  #########################################
  # FIND EcoSystem
  #########################################
  #
  # Rank users by whether they have tagged itesm a number of times similar
  # to ourself.  Generate the top <tag => count> set of my tags Do this by
  # UserTagStat (maintaining a per-tag count of the number of times a user
  # uses a tag).
  it 'finds users immediate ecosystem'
  it 'finds fringes of users ecosystem'
end
