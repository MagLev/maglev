require 'minitest/unit'
require 'webtools/code_browser'

MiniTest::Unit.autorun

class Xyzzy
  def initialize
  end

  protected
  def protected_method
  end

  private
  def private_method
  end
end

class TestCodeBrowser < MiniTest::Unit::TestCase
  def test_class_and_module_list
    list = WebTools::CodeBrowser.class_and_module_list
    refute_nil list
    assert_includes list['modules'], 'Object'
  end

  def test_select_module
    cb = WebTools::CodeBrowser.new
    state = cb.select_module 'Object'
    assert_equal 'Object', state[:selected_module]

    assert_includes state[:constants],         'Kernel'
#    assert_empty    state[:module_methods]  # yaml_tag sometimes shows up (after rails)
    assert_includes state[:instance_methods], 'to_s'

    # Test a class name with '::' in it
    state = cb.select_module 'FFI::StructLayout'
    refute_nil state
    assert_empty    state[:constants]
    assert_empty    state[:module_methods]
    assert_includes state[:instance_methods], 'add_field'
    # test that
  end

  def test_all_methods_retrieved
    cb = WebTools::CodeBrowser.new
    state = cb.select_module 'Xyzzy'
    refute_nil state
    assert_equal 'Xyzzy', state[:selected_module]
    assert_includes state[:instance_methods], 'initialize'
    assert_includes state[:instance_methods], 'protected_method'
    assert_includes state[:instance_methods], 'private_method'
  end
end
