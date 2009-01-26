          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
 All files in this distribution are subject to the terms of the Ruby license.

# About Ramaze

Ramaze is a very simple and straight-forward web-framework.
The philosophy of it could be expressed in a mix of KISS and POLS, trying to
make simple things simple and complex things possible.

This of course is nothing new to anyone who knows some ruby, but is often
forgotten in a chase for new functionality and features. Ramaze only tries to
give you the ultimate tools, but you have to use them yourself to achieve
perfect custom-tailored results.

Another one of the goals during development of Ramaze was to make every part as
modular and therefor reuasable as possible, not only to provide a basic
understanding after the first glance, but also to make it as simple as possible
to reuse parts of the code.

The original purpose of Ramaze was to act as a kind of framework to build
web-frameworks, this was made obsolete by the introduction of rack, which
provides this feature at a better level without trying to enforce any structural
layout of the resulting framework.


# Features Overview

Ramaze offers following features at the moment:

* Adapters

   Ramaze takes advantage of the rack library to provide a common way of
   handling different ways to serve its content.

   Rack supports at the moment:

    * [Mongrel](http://mongrel.rubyforge.org/)

      Mongrel is a fast HTTP library and server for Ruby that is intended for
      hosting Ruby web applications of any kind using plain HTTP rather than
      FastCGI or SCGI.

    * [WEBrick](http://www.webrick.org/)

      WEBrick is a Ruby library program to build HTTP servers.

    * CGI

      CGI is the Common Gateway Interface and is one of the most basic ways
      to integrate into Webservers like Apache or Lighttpd.

    * FCGI

      Improvment of CGI as it doesn't start up a new connection to Ramaze on
      every request.


* Templates
  * [Amrita2](http://amrita2.rubyforge.org/)

    Amrita2 is a xml/xhtml template library for Ruby. It makes html documents
    from a template and a model data.

  * [Erubis](http://rubyforge.org/projects/erubis)

    Erubis is a fast, secure, and very extensible implementation of eRuby.

  * [Haml](http://haml.hamptoncatlin.com/)

    Haml takes your gross, ugly templates and replaces them with veritable Haiku.

  * [Liquid](http://home.leetsoft.com/liquid)

    Liquid's syntax and parse model are inspired by Django templates, as well
    as PHP's smarty.

  * [Remarkably](http://rubyforge.org/projects/remarkably)

    Remarkably is a very tiny Markaby-like XML builder

  * [Markaby](http://code.whytheluckystiff.net/markaby/)

    Markaby means Markup as Ruby.

  * [Sass](http://haml.hamptoncatlin.com/docs/sass)

    Sass is a meta-language on top of CSS that‘s used to describe the style of
    a document cleanly and structurally, with more power than flat CSS allows.

  * Ezamar

    A simple homage to [Nitro](http://nitroproject.org)s templating, is shipped
    together with Ramaze.

* Cache
  * Hash
  * YAML::Store
  * MemCache

* Helper
  * Active by default
    * CGI

      Shortcuts for escape/unescape of the CGI module.

    * File

      Helps you serving files from your Controller.

    * Flash

      Store a couple of values for one request associated with a session.

    * Link

      Easier linking to the various parts of your applications Controllers and
      Actions.

    * Redirect

      Easy redirection.

  * Optional
    * Aspect

      Allows you to wrap different Actions on your Controller with code.

    * Auth

      Simple way to add basic authentication.

    * Cache

      Easy caching Actions and values.

    * Identity

      For ease of use of the OpenID authentication mechanism.

    * Inform

      Wrapping the functionality of Ramazes logging facilities.

    * Markaby

      Allows you to use Markaby in your Controller without having it as the
      default templating engine.

    * Nitroform

      Hooks up on nitros form builder to help you creating forms from Og
      objects.

    * OpenID

      Authentication via OpenID made easy.

    * Pager

      Displays a collection of entitities in multiple pages.

    * Partial

      Renders so-called partials.

    * Stack

      Allows you to use a call/answer mechanism for things like redirection to the
      site a user entered login-forms from.

* Various
  * Sessions
  * Global configuration system
  * Simple request/response handling
  * Custom sophisticated Error-handling


# Basic Principles

There are some basic principles that Ramaze tries to follow:

* KISS (Keep It Super Simple)

  Ramaze doesn't introduce any major change of paradigm for everyone familiar
  with Ruby and the basics of Web-development.

* POLS (Principle Of Least Surprise)

  Ramaze tries to be intuitive and easy to learn. Most functionality is built in
  a way to help, not to obfuscate or confuse.

* Modular design

  Use what you want and how you want it.

  Through Ruby Ramaze provides one of the most powerful programming-languages
  available, giving you full control over your system.

  Even the most essential parts of Ramaze can easily be replaced and/or modified
  without losing the advantage of the whole framework.

* Minimal dependencies

  Nothing besides Ruby is required for the basic features.

  Of course you can take advantage of several wonderful libraries, but Ramaze is
  built in a way to be run on any basic setup.

* Documentation

  Document everything, classes, modules, methods, configuration...

  Through 100% documentation Ramaze gives the developer easy and solid
  understanding of the underlying concepts and functionality.

* Open development

  Everyone is welcome to contribute to Ramaze in the easiest way possible. The
  repository is open for patches passing the Test-suite.

* Examples

  Everyone learns different, some only read the source, others browse
  documentation, but everyone loves examples for a quick and painless start.

  Ramaze addresses this need and offers a wide variety of examples of usage,
  basic functionality, project-layout and more advanced applications.

* Fully BDD (Behaviour Driven Design)

  Ramaze has a very complete set of so-called specifications built by RSpec.
  These specs define the way Ramaze has to behave.

  The specs are checked every time a new patch is pushed into the repository,
  deciding whether the changes the patch applies are valid and don't break the framework.


# Installation

* via RubyGems

  The simplest way of installing Ramaze is via the gem.

  [Rubygems](http://rubygems.org) is the package manager for ruby apps and
  libraries and provides you with the last tagged version of Ramaze.

      $ gem install ramaze

  Versions are made as we see fit and get an announcment out (usually that's
  the major obstacle as there is a lot to announce).

  If you want to install the nightly gem of Ramaze you can do this with:

      $ gem install ramaze --source=http://gem.ramaze.net/

  We also use the gem building process on github, which locates gems at
  http://gems.github.com - so you can get a version from there as well:

      $ gem install manveru-ramaze --source=http://gems.github.com/

* via git

  To get the latest and sweetest, you can just pull from the repository and run
  Ramaze that way.

      $ git clone git://github.com/manveru/ramaze.git

  Please read the `man git` or `git help` for more information about updating
  and creating your own patches.
  This is at the moment the premier way to use Ramaze, since it is the way I
  use it.

  Some hints for the usage of Git.

  * use `require 'ramaze'` from everywhere

    Simply add the path to your repository to the RUBYLIB environment variable.
    If you are using bash you can simply put following line into your .bashrc:

        $ export RUBYLIB="$HOME/path/to/repo/lib/"

    Of course you should put the real path instead, you can also add multiple
    paths, or create your personal `site_ruby`:

        $ export RUBYLIB="$HOME/ruby/ramaze/lib:$HOME/.site_ruby:$HOME/ruby/bacon/lib"

  * use `require 'ramaze'` systemwide from everywhere

    add a file to your `site_ruby` directory named 'ramaze.rb'
    the content should be: `require '/path/to/git/repo/ramaze/lib/ramaze'`

  * Pull the latest version

        $ git pull

  * Reset the repo to original state (if you screw up something)

        $ git reset --hard # resets the whole repo

  * Revert changes to (for example) lib/ramaze.rb only

        $ git checkout master lib/ramaze.rb

  * Add and commit all changes.

        $ git commit -a

  * Adding only specific changes.

        # Add hunks you want to commit to the staging area (index)
        $ git add -p

  * Commit the changes into the history of your repository

        # Create a commit from the hunks added
        $ git commit

  * output your patches into a bundle ready to be mailed (compress it before
    sending to make sure it arrives in the way you sent it)

    At the end of this process you will have a tar.bz2 for all your changes
    that you can mail to ramaze@googlegroups.com

        # make sure you are on latest revision to avoid conflicts
        $ git pull

        # create 00xx-blah.patch files against the remote repo
        $ git format-patch origin/HEAD

        # From here on you can use either git-send-email or go the manual route
        $ tar -cjf ramaze_bundle.tar.bz2 *.patch


# Getting Started

Now that you have a vague idea of what you're about to get into you might just
want to get a way to get up and running ASAP.
Please read below for more information about installation.

Depending on what you are planning to do you can either just go and start
reading the source or directly get some hands-on experience by trying some of
the examples.
Most things will require dependencies though. The basic functionality is
provided by the WEBrick adapter and the Template::Ramaze, which just run out
of the box. For more features you will have to install some templating-engines
and mongrel (_very_ recommended). Ramaze will inform you when it needs further
dependencies, so just go and try some things.

Some places to get started are:
- Read the documentation.
- Run and read the test cases.
- Look at the examples and run/modify them.



# A couple of Examples

There are some examples for your instant pleasure inside the examples-directory
in the Ramaze-distribution.
To start up an example, you can use the Ramaze binary located in bin/ramaze
for example:

  $ ramaze examples/hello.rb

Or:

  $ cd examples/blog
  $ ramaze

Since ramaze uses the start.rb by default if you don't pass anything else.

For more information about the usage of ramaze try:

  $ ramaze --help


Examples include:

* examples/hello.rb
  Hello, World!

* examples/simple.rb
  A bit more advanced than the hello-example, but still very basic.

* examples/blog
  Not yet fully functional, but coming along.

* examples/whywiki
  A basic examples of a minimalistic application, based on the Wiki of \_why in
  his camping-framework.

* examples/templates
  examples of real usage of the templating-engines. Tries to implement the same
  functionality in each `template_*.rb` file using a different engine.



# How to find Help

For help you can:

- Visit us in the channel #ramaze on irc.freenode.net

- Join the Mailinglist at http://ramaze.rubyforge.org


# Appendix

* Performance
  * Serving

    For best performance you should consider using Mongrel to host your
    application.

  * Caching

    You can easily cache your pages using the CacheHelper.
    Also, using MemCache gives you high-end performance and security.


# And thanks to...

There is a large number of people who made Ramaze possibe by their ongoing
efforts in the world of open source and by encouraging and helping me.

This list is by no means a full listing of all these people, but I try to
get a good coverage despite that.

I would like to thank:

* Yukihiro Matsumoto a.k.a matz

    For giving the world Ruby and bringing fun back into programming.

* Zed Shawn a.k.a. zedas

    For developing Mongrel, Ramaze started out as a simple Hello World based
    on that awesome server.

* Christian Neukirchen a.k.a chris2

    For building rack, which is just what the numerous web-developers had
    anticipated and which will, with no doubt, change the world.

* Pistos

    For continious encouragment and building the first real webpage on Ramaze.
    His bugreports were invaluable.

* Jim Weirich

    For Rake, which lifts off a lot of tasks from the shoulders of every
    developer who uses it.

* Thomas Sawyer a.k.a Trans

    Dragging me deep into the rabbit-hole and showing me how awesome Ruby
    truely is through his work on facets, ratchets and tons of other projects.

* George Moschovitis a.k.a gmosx

    For his tremendous efforts in the Nitro/Og framework, which is a source of
    steady inspiration for Ramaze and brought me to Ruby in the first place.

* Rob Levin a.k.a. lilo

    He founded the most excellent Freenode IRC-network, where the most important
    channels for rubyists are located (as is #ramaze).
    May he rest in peace.

* The guys (and gals) in the various channels on Freenode

    As the people are way too many to be listed, here the channels that i call
    my online home.
    All the people in there deserve special thanks for getting me hooked to Ruby
    and providing their help in a friendly and patient manner.

    * #nitro
    * #ruby-de
    * #ruby-lang
    * #rubyforce
