# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{bundler}
  s.version = "1.0.10"

  s.required_rubygems_version = Gem::Requirement.new(">= 1.3.6") if s.respond_to? :required_rubygems_version=
  s.authors = ["Carl Lerche", "Yehuda Katz", "Andr\303\203\302\251 Arko", "Terence Lee"]
  s.date = %q{2011-02-01}
  s.default_executable = %q{bundle}
  s.description = %q{Bundler manages an application's dependencies through its entire life, across many machines, systematically and repeatably}
  s.email = ["carlhuda@engineyard.com"]
  s.executables = ["bundle"]
  s.files = ["spec/cache/gems_spec.rb", "spec/cache/git_spec.rb", "spec/cache/path_spec.rb", "spec/cache/platform_spec.rb", "spec/install/deploy_spec.rb", "spec/install/deprecated_spec.rb", "spec/install/gems/c_ext_spec.rb", "spec/install/gems/env_spec.rb", "spec/install/gems/flex_spec.rb", "spec/install/gems/groups_spec.rb", "spec/install/gems/packed_spec.rb", "spec/install/gems/platform_spec.rb", "spec/install/gems/resolving_spec.rb", "spec/install/gems/simple_case_spec.rb", "spec/install/gems/sudo_spec.rb", "spec/install/gems/win32_spec.rb", "spec/install/gemspec_spec.rb", "spec/install/git_spec.rb", "spec/install/invalid_spec.rb", "spec/install/path_spec.rb", "spec/install/upgrade_spec.rb", "spec/lock/git_spec.rb", "spec/lock/lockfile_spec.rb", "spec/other/check_spec.rb", "spec/other/config_spec.rb", "spec/other/console_spec.rb", "spec/other/exec_spec.rb", "spec/other/ext_spec.rb", "spec/other/gem_helper_spec.rb", "spec/other/help_spec.rb", "spec/other/init_spec.rb", "spec/other/newgem_spec.rb", "spec/other/open_spec.rb", "spec/other/show_spec.rb", "spec/pack/gems_spec.rb", "spec/quality_spec.rb", "spec/resolver/basic_spec.rb", "spec/resolver/platform_spec.rb", "spec/runtime/executable_spec.rb", "spec/runtime/load_spec.rb", "spec/runtime/platform_spec.rb", "spec/runtime/require_spec.rb", "spec/runtime/setup_spec.rb", "spec/runtime/with_clean_env_spec.rb", "spec/spec_helper.rb", "spec/support/builders.rb", "spec/support/helpers.rb", "spec/support/indexes.rb", "spec/support/matchers.rb", "spec/support/path.rb", "spec/support/platforms.rb", "spec/support/ruby_ext.rb", "spec/support/rubygems_ext.rb", "spec/support/rubygems_hax/platform.rb", "spec/support/sudo.rb", "spec/update/gems_spec.rb", "spec/update/git_spec.rb", "spec/update/source_spec.rb", "bin/bundle"]
  s.homepage = %q{http://gembundler.com}
  s.require_paths = ["lib"]
  s.rubyforge_project = %q{bundler}
  s.rubygems_version = %q{1.6.0}
  s.summary = %q{The best way to manage your application's dependencies}
  s.test_files = ["spec/cache/gems_spec.rb", "spec/cache/git_spec.rb", "spec/cache/path_spec.rb", "spec/cache/platform_spec.rb", "spec/install/deploy_spec.rb", "spec/install/deprecated_spec.rb", "spec/install/gems/c_ext_spec.rb", "spec/install/gems/env_spec.rb", "spec/install/gems/flex_spec.rb", "spec/install/gems/groups_spec.rb", "spec/install/gems/packed_spec.rb", "spec/install/gems/platform_spec.rb", "spec/install/gems/resolving_spec.rb", "spec/install/gems/simple_case_spec.rb", "spec/install/gems/sudo_spec.rb", "spec/install/gems/win32_spec.rb", "spec/install/gemspec_spec.rb", "spec/install/git_spec.rb", "spec/install/invalid_spec.rb", "spec/install/path_spec.rb", "spec/install/upgrade_spec.rb", "spec/lock/git_spec.rb", "spec/lock/lockfile_spec.rb", "spec/other/check_spec.rb", "spec/other/config_spec.rb", "spec/other/console_spec.rb", "spec/other/exec_spec.rb", "spec/other/ext_spec.rb", "spec/other/gem_helper_spec.rb", "spec/other/help_spec.rb", "spec/other/init_spec.rb", "spec/other/newgem_spec.rb", "spec/other/open_spec.rb", "spec/other/show_spec.rb", "spec/pack/gems_spec.rb", "spec/quality_spec.rb", "spec/resolver/basic_spec.rb", "spec/resolver/platform_spec.rb", "spec/runtime/executable_spec.rb", "spec/runtime/load_spec.rb", "spec/runtime/platform_spec.rb", "spec/runtime/require_spec.rb", "spec/runtime/setup_spec.rb", "spec/runtime/with_clean_env_spec.rb", "spec/spec_helper.rb", "spec/support/builders.rb", "spec/support/helpers.rb", "spec/support/indexes.rb", "spec/support/matchers.rb", "spec/support/path.rb", "spec/support/platforms.rb", "spec/support/ruby_ext.rb", "spec/support/rubygems_ext.rb", "spec/support/rubygems_hax/platform.rb", "spec/support/sudo.rb", "spec/update/gems_spec.rb", "spec/update/git_spec.rb", "spec/update/source_spec.rb"]

  if s.respond_to? :specification_version then
    s.specification_version = 3

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<ronn>, [">= 0"])
      s.add_development_dependency(%q<rspec>, [">= 0"])
    else
      s.add_dependency(%q<ronn>, [">= 0"])
      s.add_dependency(%q<rspec>, [">= 0"])
    end
  else
    s.add_dependency(%q<ronn>, [">= 0"])
    s.add_dependency(%q<rspec>, [">= 0"])
  end
end
