#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

spec_require 'liquid'

module ProductsFilter
  def price(integer)
    sprintf("$%.2d USD", integer / 100.0)
  end

  def prettyprint(text)
    text.gsub( /\*(.*)\*/, '<b>\1</b>' )
  end

  def count(array)
    array.size
  end

  def paragraph(p)
    "<p>#{p}</p>"
  end
end


class TCTemplateLiquidController < Ramaze::Controller
  map '/'
  view_root 'spec/ramaze/template/liquid/'
  engine :Liquid
  trait :liquid_options => { :filters => ProductsFilter }

  def index
    @hash = {'name' => 'tobi'}
  end

  def products
    @hash = {'products' => products_list, 'section' => 'Gems', 'cool_products' => true}
  end

  private

  def products_list
    [
      {'name' => 'Ruby', 'price' => 1000, 'description' => 'precious gem'},
      {'name' => 'Diamond', 'price' => 2000, 'description' => 'even more precious gem'},
    ]
  end
end

describe "Liquid" do
  behaves_like 'http'
  ramaze

  it "index" do
    get('/').body.strip.should == "hi tobi"
  end

  it "products" do
    o = get('/products')
    o = o.body.split("\n").map{|l| l.strip!; l.empty? ? nil : l}.compact.join("\n")
    o.should == %{
      <?xml version="1.0" encoding="utf-8"?>
      <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
      <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
        <head>
          <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
          <meta http-equiv="Content-Language" content="en-us" />
          <title>products</title>
          <meta name="ROBOTS" content="ALL" />
          <meta http-equiv="imagetoolbar" content="no" />
          <meta name="MSSmartTagsPreventParsing" content="true" />
          <meta name="Copyright" content="(c) 2005 Copyright content:  Copyright design: Tobias Luetke" />
          <!-- (c) Copyright 2005 by Tobias Luetke All Rights Reserved. -->
        </head>
        <body>
          <h1>There are currently 2 products in the Gems catalog</h1>
          Cool products :)
          <ul id="products">
            <li>
              <h2>Ruby</h2>
              Only $10 USD
              <p>precious gem</p>
              <p>it rocks!</p>
            </li>
            <li>
              <h2>Diamond</h2>
              Only $20 USD
              <p>even more precious gem</p>
              <p>it rocks!</p>
            </li>
          </ul>
        </body>
      </html>
      }.split("\n").map{|l| l.strip!; l.empty? ? nil : l}.compact.join("\n")
  end
end

