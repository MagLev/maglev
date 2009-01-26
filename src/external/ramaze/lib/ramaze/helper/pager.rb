module Ramaze

# The BSD License
#
# Copyright (c) 2004-2007, George K. Moschovitis. (http://www.gmosx.com)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# * Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# * Neither the name of Nitro nor the names of its contributors may be
# used to endorse or promote products derived from this software
# without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# Displays a collection of entitities in multiple pages.
#
# === Design
#
# This pager is carefully designed for scaleability. It stores only the items
# for one page. The key parameter is needed, multiple pagers can coexist in a
# single page. The pager leverages the SQL LIMIT option to optimize database
# interaction.
#
#
# === Example
#
#   class MyController
#     def index
#       objs = (0..200).to_a
#       @entries, @pager = paginate(objs, :limit => 20)
#     end
#   end
#
#
#   <html>
#     <head><title>Pager</title></head>
#     <body>
#       <?r if pager.navigation? ?>
#         <div class="pager">#{@pager.navigation}</div>
#       <?r end ?>
#       <ul>
#       <?r @entries.each do |entry| ?>
#         <li>#{entry}</li>
#       <?r end ?>
#       </ul>
#     </body>
#   </html>
#
# === Styling
#
# The following classes can be used for styling with CSS (provided you put the
# pager in a element with class 'pager' like shown above):
#
#   .pager {}
#   .pager .first {}
#   .pager .previous {}
#   .pager .next {}
#   .pager .last {}
#   .pager ul {}
#   .pager li {}
#   .pager li.active {}

class Pager
  include Ramaze::Helper::Link

  # Items per page.

  trait :limit => 10

  # The request key.

  trait :key => '_page'

  # The current page.

  attr_reader :page

  # Items per page.

  attr_reader :limit

  # The total number of pages.

  attr_reader :page_count

  # Total count of items.

  attr_reader :total_count

  # Create a new Pager object.
  #
  # request:: Ramaze::Request object providing access to GET parameters
  # limit:: how many elements go to one page
  # total_count:: total element count
  # key:: key used for getting the current page from GET paramaters
  #
  # Note:  You never have to create this class yourself, use the `paginate()`
  # convenience method from the Helper::Pager.

  def initialize(request, limit, total_count, key = trait[:key])
    raise 'limit should be > 0' unless limit > 0

    @request, @key = request, key
    @page = (request.params[key] || 1).to_i
    @limit = limit
    set_count(total_count)
    @start_idx = (@page - 1) * limit
  end

  # Return the first page index.

  def first_page
    1
  end

  # Is the first page displayed?

  def first_page?
    @page == 1
  end

  # Return the last page index.

  def last_page
    return @page_count
  end

  # Is the last page displayed?

  def last_page?
    @page == @page_count
  end

  # Return the index of the previous page.

  def prev_page
    [@page - 1, 1].max
  end

  # Return the index of the next page.

  def next_page
    [@page + 1, @page_count].min
  end

  # Returns each element for the current page

  def each(&block)
    @page_items.each(&block)
  end

  # Iterator
  # Returns 1-based index.

  def each_with_index(&block)
    @page_items.each_with_index(&block)
  end

  # Is the pager empty, ie has one page only?

  def empty?
    @page_count < 2
  end

  # Returns true if a navigation is necessary (meaning there is more than one
  # page)

  def navigation?
    !empty?
  end

  # Returns the amount of all elements in all pages.

  def size
    @total_count
  end

  # Override this method in your application if needed.
  #--
  # TODO: better markup.
  #++

  def navigation
    nav = ""

    unless first_page?
      nav << %{
        <div class="first"><a href="#{link_first_page}">First</a></div>
        <div class="previous"><a href="#{link_prev_page}">Previous</a></div>
      }
    end

    unless last_page?
      nav << %{
        <div class="last"><a href="#{link_last_page}">Last</a></div>
        <div class="next"><a href="#{link_next_page}">Next</a></div>
      }
    end

    nav << %{<ul>}

    for i in nav_range()
      if i == @page
        nav << %{<li class="active">#{i}</li>}
      else
        nav << %{<li><a href="#{target_uri(i)}">#{i}</a></li>}
      end
    end

    nav << %{</ul>}

    return nav
  end

  # To be used with Og queries.

  def limit
    if @start_idx > 0
      { :limit => @limit, :offset => @start_idx }
    else
      { :limit => @limit }
    end
  end

  # Returns the index of the first element to go into the current page

  def offset
    @start_idx
  end

private

  # Generate the target URI.

  def target_uri(page)
    params = @request.params.merge(@key => page)
    Rs(Action.current.name, params)
  end

  # Generate link for the first page.

  def link_first_page; target_uri(first_page); end

  # Generate link for the last page.

  def link_last_page; target_uri(last_page); end

  # Generate link for the previous page.

  def link_prev_page; target_uri(prev_page); end

  # Generate link for the next page.

  def link_next_page; target_uri(next_page); end


  # Returns the range of the current page.

  def page_range
    s = @idx
    e = [@idx + @items_limit - 1, all_total_count].min

    return [s, e]
  end

  # Returns the range of
  # Override if needed.

  def nav_range
    # effective range = 10 pages.
    s = [@page - 5, 1].max
    e = [@page + 9, @page_count].min

    d = 9 - (e - s)
    e += d if d < 0

    return (s..e)
  end

  # generates total and page count of the pager.

  def set_count(total_count)
    @total_count = total_count
    @page_count = (@total_count.to_f / @limit).ceil
  end

end

# Pager related helper methods.

module Helper::Pager

  # Helper method that generates a collection of items and the
  # associated pager object.
  #
  # === Example
  #
  # items = [ 'item1', 'item2', ... ]
  # entries, pager = paginate(items, :limit => 10)
  #
  # <ul>
  # <?r for entry in entries ?>
  #    <li>#{entry.to_link}</li>
  # <?r end ?>
  # </ul>
  # #{pager.navigation}
  #
  # === Og Example
  #
  # entries, pager = paginate(Article, :where => 'title LIKE..', :limit => 10)
  #
  # or
  #
  # entries, pager = paginate(article.comments, :limit => 10)

  def paginate(items, options = {})
    limit = options.delete(:limit) || Pager.trait[:limit]
    pager_key = options.delete(:pager_key) || Pager.trait[:key]

    case items
    when Array
      pager = Pager.new(request, limit, items.size, pager_key)
      items = items.slice(pager.offset, pager.limit[:limit])
      return items, pager
    end

    if defined?(Og) && items.is_a?(Og::Collection)
      pager = Pager.new(request, limit, items.count, pager_key)
      options.update(pager.limit)
      items = items.reload(options)
      return items, pager
    elsif defined?(Og::EntityMixin) && items.is_a?(Og::EntityMixin) ||
          defined?(Og::Mixin) && items.is_a?(Og::Mixin) # Og <= 0.41
      pager = Pager.new(request, limit, items.count(options), pager_key)
      options.update(pager.limit)
      items = items.all(options)
      return items, pager
    end

    raise "No suitable pagination method for #{items.inspect}"
  end

end

end
