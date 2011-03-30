require 'minitest/unit'
require 'webtools/code_browser'

MiniTest::Unit.autorun

class TestCodeBrowser < MiniTest::Unit::TestCase
  def test_class_and_module_list
    list = WebTools::CodeBrowser.class_and_module_list
    refute_nil list
    assert_includes list['classNames'], 'Object'
  end

  def test_select_module
    cb = WebTools::CodeBrowser.new
    state = cb.select_module 'Object'
    assert_equal 'Object', state[:selected_class]

    assert_includes state[:constants],         'Kernel'
    assert_empty    state[:module_methods]
    assert_includes state[:instance_methods], 'to_s'

    # Test a class name with '::' in it
    state = cb.select_module 'FFI::StructLayout'
    refute_nil state
    assert_empty    state[:constants]
    assert_empty    state[:module_methods]
    assert_includes state[:instance_methods], 'add_field'
  end
end
