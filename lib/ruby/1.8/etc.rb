# -*- coding: utf-8 -*-

Group  = Struct.new(:name, :passwd, :gid, :mem)
Passwd = Struct.new(:name, :passwd, :uid, :gid, :gecos, :dir, :shell)

# The etc module provides access to information from the /etc/passwd and
# /etc/group files on Linux and Unix systems.
#
# Documented by mathew <meta@pobox.com>.
#
# Note: the calls to Etc.getgrent, Etc.getpwent, Etc.group and Etc.passwd
# are not thread safe.
module Etc

  @restart_gr = true
  @restart_pw = true

  # Returns an entry from the /etc/group file. The first time it is called
  # it opens the file and returns the first entry; each successive call
  # returns the next entry, or nil if the end of the file has been reached.
  #
  # To close the file when processing is complete, call endgrent.
  #
  # Each entry is returned as a Struct::Group:
  #
  # * Group#name contains the name of the group as a String.
  #
  # * Group#passwd contains the encrypted password as a String.
  #   An ‘x’ is returned if password access to the group is not available;
  #   an empty string is returned if no password is needed to obtain
  #    membership of the group.
  # * Group#gid contains the group‘s numeric ID as an integer.
  # * Group#mem is an Array of Strings containing the short login names of
  #   the members of the group.
  def self.getgrent
    result = if @restart_gr
               @restart_gr = false
               Dir.__getgrgent(0)
             else
               Dir.__getgrgent(1)
             end
    result.nil? ? nil : Struct::Group.new(*result)
  end

  # Ends the process of scanning through the /etc/group file begun by
  # getgrent
  def self.endgrent
    @restart_gr = true
  end

  # Ends the process of scanning through the /etc/passwd file begun by
  # getpwent
  def self.endpwent
    @restart_pw = true
  end

  # Returns information about the group with specified integer group id
  # (gid), as found in /etc/group.
  #
  # The information is returned as a Struct::Group; see getgrent above for
  # details.  E.g.
  # Etc.getgrgid(100) -> #<struct Struct::Group name="users", passwd="x", gid=100, mem=["meta", "root"]>
  def self.getgrgid(gid)
    raise TypeError, "can't convert #{gid.class} into Integer" unless gid.is_a?(Integer)
    result = Dir.__getgrgid(gid)
    raise ArgumentError, "can't find group for #{gid}" if result.nil?
    Group.new(*result)
  end

  # Returns information about the group with specified String name, as
  # found in /etc/group.
  #
  # The information is returned as a Struct::Group; see getgrent above for
  # details.  E.g.
  # Etc.getgrnam(‘users’) -> #<struct Struct::Group name="users", passwd="x", gid=100, mem=["meta", "root"]>
  def self.getgrnam(name)
    raise TypeError, "can't convert #{name.class} into String" unless name.is_a?(String)
    result = Dir.__getgrnam(name)
    raise ArgumentError, "can't find group for #{name}" if result.nil?
    Group.new(*result)
  end

  # Returns the short user name of the currently logged in user.
  def self.getlogin
    Dir.__getlogin
  end

  # Returns an entry from the /etc/passwd file. The first time it is called
  # it opens the file and returns the first entry; each successive call
  # returns the next entry, or nil if the end of the file has been reached.
  #
  # To close the file when processing is complete, call endpwent.
  #
  # Each entry is returned as a Struct::Passwd:
  #
  # * Passwd#name contains the short login name of the user as a String.
  #
  # * Passwd#passwd contains the encrypted password of the user as a
  #   String. an ‘x’ is returned if shadow passwords are in use. An ’*’ is
  #   returned if the user cannot log in using a password.
  #
  # * Passwd#uid contains the integer user ID (uid) of the user.
  # * Passwd#gid contains the integer group ID (gid) of the user‘s
  #   primary group.
  # * Passwd#gecos contains a longer String description of the user, such
  #   as a full name. Some Unix systems provide structured information in
  #   the gecos field, but this is system-dependent.
  # * Passwd#dir contains the path to the home directory of the user as
  #   a String.
  # * Passwd#shell contains the path to the login shell of the user as
  #   a String.
  def self.getpwent
    result = if @restart_pw
               @restart_pw = false
               Dir.__getpwent(0)
             else
               Dir.__getpwent(1)
             end
    result.nil? ? nil : Struct::Passwd.new(*result)
  end

  # Returns the /etc/passwd information for the user with specified login
  # name.  The information is returned as a Struct::Passwd; see getpwent
  # above for details. E.g.,
  # Etc.getpwnam(‘root’) -> #<struct Struct::Passwd name="root", passwd="x", uid=0, gid=0, gecos="root",dir="/root", shell="/bin/bash">
  def self.getpwnam(name)
    raise TypeError, "can't convert #{name.class} into String" unless name.is_a?(String)
    result = Dir.__getpwnam(name)
    raise ArgumentError, "can't find user for #{name}" if result.nil?
    Passwd.new(*result)
  end

  # Returns the /etc/passwd information for the user with specified uid.
  # If uid is nil, the uid of the current user is used.
  # The information is returned as a Struct::Passwd; see getpwent
  # above for details. E.g.,
  # Etc.getpwnam(0) -> #<struct Struct::Passwd name="root", passwd="x", uid=0, gid=0, gecos="root",dir="/root", shell="/bin/bash">
  def self.getpwuid(uid=nil)
    if uid.nil?
      uid = Dir.__getuid
    elsif not uid.is_a?(Integer)
      raise TypeError, "can't convert #{uid.class} into Integer"
    end

    result = Dir.__getpwuid(uid)
    raise ArgumentError, "can't find user for #{uid}" if result.nil?
    Passwd.new(*result)
  end

  # Provides a convenient Ruby iterator which executes a block for each
  # entry in the /etc/group file.
  def self.group
    self.endgrent
    while (not (result = self.getgrent).nil?)
      yield result
    end
  end

  # Provides a convenient Ruby iterator which executes a block for each
  # entry in the /etc/passwd file.
  def self.passwd
    self.endpwent
    while (not (result = self.getpwent).nil?)
      yield result
    end
  end

  class << self
    alias setgrent endgrent
    alias setpwent endpwent
  end
end
