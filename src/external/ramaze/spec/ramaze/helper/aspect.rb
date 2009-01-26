#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'spec/helper'

class TCAspectController < Ramaze::Controller
  map '/'
  trait :foo => :bar
  helper :aspect

  def test_before() $helper_aspect_spec_test_before += 2 end
  before(:test_before){ $helper_aspect_spec_test_before = 40 }

  def test_after() $helper_aspect_spec_test_after = 40 end
  after(:test_after){ $helper_aspect_spec_test_after += 2 }

  def test_wrap() end
  wrap(:test_wrap){ $helper_aspect_spec_test_wrap ||= 0; $helper_aspect_spec_test_wrap += 21 }

  wrap(:test_template) { '<aspect>' }
end

class TCAspectAllController < Ramaze::Controller
  map '/all'

  helper :aspect
  view_root __DIR__(:view)

  def test_all_first() 'first' end
  def test_all_second() 'second' end

  before_all{ $helper_aspect_spec_all = 40 }
  after_all{ $helper_aspect_spec_all += 2 }

  def test_all_after() 'after' end

  def layout() '<div>#@content</div>' end
  template :loop_with_layout, :loop
  layout :layout => [:loop_with_layout]
end

describe "AspectHelper" do
  behaves_like 'http'
  ramaze :error_page => false

  it "shouldn't overwrite traits on inclusion" do
    TCAspectController.trait[:foo].should == :bar
  end

  it 'should use before' do
    $helper_aspect_spec_test_before = nil
    get('/test_before')
    $helper_aspect_spec_test_before.should == 42
  end

  it 'should use after' do
    $helper_aspect_spec_test_after = nil
    get('/test_after')
    $helper_aspect_spec_test_after.should == 42
  end

  it 'should use wrap' do
    $helper_aspect_spec_test_wrap = nil
    get('/test_wrap')
    $helper_aspect_spec_test_wrap.should == 42
  end

  it 'should before_all and after_all' do
    $helper_aspect_spec_all = nil
    get('/all/test_all_first')
    $helper_aspect_spec_all.should == 42

    $helper_aspect_spec_all = nil
    get('/all/test_all_second')
    $helper_aspect_spec_all.should == 42
  end

  it 'should before_all and after_all for templates' do
    $helper_aspect_spec_all = nil
    get('/all/test_template')
    $helper_aspect_spec_all.should == 42
  end

  it 'should before_all and after_all for all defined actions' do
    $helper_aspect_spec_all = nil
    get('/all/test_all_after')
    $helper_aspect_spec_all.should == 42
  end

  it 'should not apply aspects to render_template' do
    $helper_aspect_spec_all = nil
    get('/all/loop').body.gsub(/\s/,'').should == '12345'
    $helper_aspect_spec_all.should == 42
  end

  it 'should not apply aspects to layouts' do
    $helper_aspect_spec_all = nil
    get('/all/loop_with_layout').body.gsub(/\s/,'').should == '<div>12345</div>'
    $helper_aspect_spec_all.should == 42
  end
end
