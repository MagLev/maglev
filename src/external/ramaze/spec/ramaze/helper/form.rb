require 'spec/helper'

spec_require 'hpricot', 'sequel'

DB = Sequel.sqlite

class User < Sequel::Model(:user)
  set_schema do
    primary_key :id

    boolean :online
    varchar :name
    integer :level
    text :description
    date :birthday
    time :created
  end
end

User.create_table

class FormController < Ramaze::Controller
  map '/'
  helper :form

  def new
    form_for(User).to_s
  end

  def new_with_options
    form_for(User, :method => :POST, :action => Rs(:create)).to_s
  end

  def edit(id)
    form_for(User[id])
  end

  def edit_with_options(id)
    form_for(User[id], :method => :POST, :action => Rs(:create)).to_s
  end
end

describe 'Helper::Form' do
  ramaze

  describe 'raw model' do
    behaves_like 'requester'

    should 'handle class' do
      form = hget('/new').at(:form)
      inputs = (form/:input)
      inputs.map{|i| i[:name] }.compact.sort.should == %w[level name online]
      form.at(:textarea)[:name].should == 'description'
    end

    should 'handle options' do
      hget('/new_with_options').at(:form).raw_attributes.
        should == {"action" => "/create", "method" => "POST"}
    end
  end

  describe 'instances' do
    behaves_like 'requester'

    data = {
      :name        => 'manveru',
      :description => 'Ramaze dev',
      :online      => true,
      :level       => 2,
      :birthday    => Time.now,
      :created     => Time.now,
    }
    User.create data

    should 'handle class' do
      form = hget('/edit/1').at(:form)

      form.at('input[@name=name]').raw_attributes.should ==
        { "name" => "name", "type" => "text", "value" => "manveru"}
      form.at('input[@name=online]').raw_attributes.should ==
        {"name" => "online", "checked" => "checked", "type" => "checkbox", "value" => "true"}
      form.at('input[@name=level]').raw_attributes.should ==
        {"name" => "level", "type" => "text", "value" => "2"}

      # TODO:
      # find a way to XPATH to input[@name="birthday[day]"]
      # the [] in the name seems to break things, works fine with ()

      # check date

      date = data[:birthday]
      selects = (form/'select[@name]').select{|s| s[:name] =~ /birthday/ }

      { :day => date.day, :month => date.month, :year => date.year,
      }.each do |key, value|
        select = selects.find{|s| s[:name] == "birthday[#{key}]" }
        select.at('[@selected]')[:value].to_i.should == value
      end

      # check time

      time = data[:created]
      selects = (form/'select[@name]').select{|s| s[:name] =~ /created/ }

      { :day => time.day, :month => time.month, :year => time.year,
        :hour => time.hour, :min => time.min, :sec => time.sec,
      }.each do |key, value|
        select = selects.find{|s| s[:name] == "created[#{key}]" }
        select.at('[@selected]')[:value].to_i.should == value
      end
    end

    should 'handle options' do
      hget('/edit_with_options/1').at(:form).raw_attributes.
        should == {"action" => "/create", "method" => "POST"}
    end
  end
end
