#! C:/Ruby192/bin/ruby.exe -sWKu
# -*- coding: utf-8 -*-

#
#  Copyright 2010 Yusuke Kawakami
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

require "digest/md5"
require "thrift/types"
require "thrift/struct"
require "thrift/protocol/base_protocol"
require "thrift/protocol/binary_protocol"
require "thrift/transport/base_transport"
require "thrift/transport/http_client_transport"
require "Evernote/EDAM/user_store"
require "Evernote/EDAM/user_store_constants"
require "Evernote/EDAM/note_store"
require "Evernote/EDAM/limits_constants"

require 'optparse'
require 'cgi'
require 'logger'
require 'kconv'
require 'forwardable'


#
# main module
#
module EnClient
  APPLICATION_NAME_TEXT  = %|emacs-enclient {:version => 0.21, :editmode => "TEXT"}|
  APPLICATION_NAME_XHTML = %|emacs-enclient {:version => 0.21, :editmode => "XHTML"}|
  #EVERNOTE_HOST       = "sandbox.evernote.com"
  EVERNOTE_HOST       = "www.evernote.com"
  USER_STORE_URL      = "https://#{EVERNOTE_HOST}/edam/user"
  NOTE_STORE_URL_BASE = "http://#{EVERNOTE_HOST}/edam/note/"
  SESSION_FILE        = File.expand_path "~/.enclientsession"
  E_OK    = 0
  E_FAIL  = 100
  E_PARSE = 101

  NOTE_DEFAULT_HEADER = %|<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd"><en-note>|

  NOTE_DEFAULT_FOOTER = %|</en-note>|

  LOG = Logger.new STDOUT
  LOG.level = Logger::INFO

  #
  # Exception
  #
  class EnClientException < StandardError; end
  class IllegalArgumentException < EnClientException; end
  class IllegalStateException < EnClientException; end

  #
  # Proxy support HTTP Transport.
  #
  class HTTPWithProxyClientTransport < Thrift::BaseTransport
    def initialize(url, proxy_addr = nil, proxy_port = nil)
      @url = URI url
      @headers = {'Content-Type' => 'application/x-thrift'}
      @outbuf = ""
      @proxy_addr = proxy_addr
      @proxy_port = proxy_port
    end

    def open?; true end
    def read(sz); @inbuf.read sz end
    def write(buf); @outbuf << buf end

    def add_headers(headers)
      @headers = @headers.merge(headers)
    end

    def flush
      if @proxy_addr && @proxy_port
        http = Net::HTTP::Proxy(@proxy_addr, @proxy_port).new @url.host, @url.port
      else
        http = Net::HTTP.new @url.host, @url.port
      end
      http.use_ssl = @url.scheme == "https"
      #http.verify_mode = OpenSSL::SSL::VERIFY_PEER
      #http.verify_depth = 5
      http.verify_mode = OpenSSL::SSL::VERIFY_NONE
      resp, data = http.post(@url.request_uri, @outbuf, @headers)
      @inbuf = StringIO.new data
      @outbuf = ""
    end
  end

  #
  # Utility for S-expr
  #
  class Formatter
    extend Forwardable

    Pair = Struct.new :key, :value

    def initialize
      @elems = []
    end

    def_delegators :@elems, :each
    def_delegators :@elems, :<<

    def to_s(indent_level=0)
      self.class.to_lisp_expr(self)
    end

    private

    def self.to_lisp_expr(obj, indent_level=0)
      str = ""
      indent = " " * indent_level

      case obj
      when String
        str << %|#{indent}"#{escape obj}"\n|
      when Time
        str << %|#{indent}"#{obj}"\n|
      when Pair
        str << %|#{indent}(#{obj.key} . \n|
        str << to_lisp_expr(obj.value, indent_level + 1)
        str << %|#{indent})\n|
      when Formatter
        str << %|#{indent}(\n|
        obj.each do |elem|
          str << to_lisp_expr(elem, indent_level + 1)
        end
        str << %|#{indent})\n|
      else
        if obj == nil
          str << %|#{indent} nil\n|
        else
          str << %|#{indent} #{obj}\n|
        end
      end
    end

    def self.escape(str)
      str.gsub(/\\/,'\&\&').gsub(/"/, '\\"')
    end
  end

  #
  # Tag Information
  #
  class TagInfo
    class Node
      def initialize(tag=nil)
        @tag = tag
        @children = []
      end

      def add_child(child)
        @children << child
      end

      def get_formatter
        formatter = Formatter.new
        if @tag
          formatter << Formatter::Pair.new("guid", @tag.guid)
          formatter << Formatter::Pair.new("name", @tag.name)
          unless @children.empty?
            children = Formatter.new
            @children.each do |child|
              children << child.get_formatter
            end
            formatter << Formatter::Pair.new("children", children)
          end
        else
          unless @children.empty?
            children = Formatter.new
            @children.each do |child|
              formatter << child.get_formatter
            end
          end
        end
        formatter
      end

      def guid
        @tag.guid
      end
    end

    def initialize(tags)
      @guid_node_map = {}
      @name_node_map = {}
      @root = Node.new

      tags.each do |t|
        node = Node.new t
        @guid_node_map[t.guid] = node
        @name_node_map[t.name] = node
      end

      tags.each do |t|
        pguid = t.parentGuid
        node = @guid_node_map[t.guid]

        if pguid == nil
          @root.add_child node
        elsif @guid_node_map.key? pguid
          pnode = @guid_node_map[pguid]
          pnode.add_child node
        end
      end
    end

    def get_tag_guid(name)
      @name_node_map[name].guid if @name_node_map[name]
    end

    def get_tag_name(guid)
      @name_node_map.each do |key, value|
        if value.guid == guid
          break key
        end
      end
    end

    def print_tree
      puts @root.get_formatter.to_s
    end
  end

  #
  # Session Mananager
  #
  class SessionManager
    def new_session(user, passwd)
      if File.exists? SESSION_FILE
        File.delete SESSION_FILE
      end

      proxy_host, proxy_port = get_proxy

      if proxy_host
        user_store_transport = HTTPWithProxyClientTransport.new USER_STORE_URL, proxy_host, proxy_port
      else
        user_store_transport = HTTPWithProxyClientTransport.new USER_STORE_URL
      end
      user_store_protocol = Thrift::BinaryProtocol.new user_store_transport
      user_store          = Evernote::EDAM::UserStore::UserStore::Client.new user_store_protocol

      version_ok = user_store.checkVersion("Emacs Client",
                                           Evernote::EDAM::UserStore::EDAM_VERSION_MAJOR,
                                           Evernote::EDAM::UserStore::EDAM_VERSION_MINOR)

      unless version_ok
        error "UserStore version invalid"
        return
      end

      appname = "kawayuu"
      appid = "24b37bd1326624a0"
      auth_result = user_store.authenticate user, passwd, appname, appid

      open SESSION_FILE, File::WRONLY|File::CREAT|File::TRUNC, 0600 do |fp|
        fp.puts auth_result.authenticationToken, auth_result.user.shardId
      end
    end

    def get_note_store_and_auth_token
      auth_token, shardId = nil, nil
      open SESSION_FILE do |fp|
        auth_token, shardId = fp.readlines
      end
      auth_token.chomp!
      shardId.chomp!

      note_store_url = NOTE_STORE_URL_BASE + shardId

      proxy_host, proxy_port = get_proxy
      if proxy_host
        note_store_transport = HTTPWithProxyClientTransport.new note_store_url, proxy_host, proxy_port
      else
        note_store_transport = HTTPWithProxyClientTransport.new note_store_url
      end

      note_store_protocol  = Thrift::BinaryProtocol.new note_store_transport
      note_store = Evernote::EDAM::NoteStore::NoteStore::Client.new note_store_protocol
      [note_store, auth_token]
    end

    private

    def get_proxy
      proxy_str = ENV["EN_PROXY"]
      if proxy_str
        proxy_str =~ /((?:\w|\.)+):([0-9]+)/
        [$1, $2]
      else
        nil
      end
    end
  end

  #
  # Base Command
  #
  class Command
    def initialize(name, assume_login = true)
      @name  = name
      @opt = OptionParser.new
      @opt.program_name = @name
      @opt.banner = @name
      @session = SessionManager.new
      @assume_login = assume_login
    end

    def name
      @name
    end

    def description
      @opt.help
    end

    def exec(args)
      if @assume_login
        begin
          @note_store, @auth_token = @session.get_note_store_and_auth_token
        rescue SystemCallError # File not found
          error "not log in or session time is expired, please #{$0} login"
          return Evernote::EDAM::Error::EDAMErrorCode::INVALID_AUTH
        end
      end

      begin
        exec_impl args
      rescue
        return handle_exception($!)
      end

      E_OK
    end

    private

    def parse_args(args, num_mandatory)
      @opt.permute! args
      if args.length < num_mandatory
        raise OptionParser::MissingArgument.new("missing mandatory argument")
      elsif args.length > num_mandatory
        raise OptionParser::NeedlessArgument.new("redundant argument")
      end
      return Utils::unpack_utf8_string_list(args)
    end

    def get_text_block
      text_block = $stdin.read
      text_block.force_encoding "ASCII-8BIT" if text_block.respond_to? :force_encoding
      text_block
    end

    def create_tag_info
      TagInfo.new @note_store.listTags(@auth_token)
    end

    def handle_exception ex
      case ex
      when Evernote::EDAM::Error::EDAMUserException
        errorCode = ex.errorCode
        parameter = ex.parameter
        errorText = Evernote::EDAM::Error::EDAMErrorCode::VALUE_MAP[errorCode]
        msg = "#{ex.class.name} (parameter: #{parameter} errorCode: #{errorText})"
        error msg
        return errorCode
      when Evernote::EDAM::Error::EDAMSystemException
        errorCode = ex.errorCode
        message   = ex.message
        errorText = Evernote::EDAM::Error::EDAMErrorCode::VALUE_MAP[errorCode]
        msg = "#{ex.class.name} (message: #{message} errorCode: #{errorText})"
        error msg
        return errorCode
      when Evernote::EDAM::Error::EDAMNotFoundException
        identifier = ex.identifier
        key = ex.key
        msg = "#{ex.class.name} (identifier: #{ex.identifier} key: #{ex.key}"
        error msg
        return E_FAIL
      when EnClientException
        error ex.message
        return E_FAIL
      when SocketError
        error ex.message
        return E_FAIL
      when OptionParser::ParseError
        error ex.message
        return E_PARSE
      else
        error ex.message
        raise ex
      end
    end

    def error(msg)
      $stderr.puts msg
    end
  end

  #
  # Login Command
  #
  class Login < Command
    def self.get_command_name
      "login"
    end

    def initialize
      super self.class.get_command_name, false
      @opt.banner = "#{@name} user passwd"
    end

    def exec_impl(args)
      user, passwd = parse_args args, 2
      @session.new_session user, passwd
    end
  end

  #
  # ListNotebooks Command
  #
  class ListNotebooks < Command
    def self.get_command_name
      "listnotebooks"
    end

    def initialize
      super self.class.get_command_name
    end

    def exec_impl(args)
      parse_args args, 0
      notebooks = @note_store.listNotebooks @auth_token
      formatter = Formatter.new
      notebooks.each do |nb|
        formatter << nb.name
      end
      puts formatter.to_s
    end
  end

  #
  # ListTags Command
  #
  class ListTags < Command
    def self.get_command_name
      "listtags"
    end

    def initialize
      super self.class.get_command_name
    end

    def exec_impl(args)
      #tag_info = create_tag_info
      parse_args args, 0
      tags = @note_store.listTags @auth_token
      tags.sort! do |a, b|
        a.name <=> b.name
      end

      formatter = Formatter.new
      tags.each do |s|
        alist = Formatter.new
        alist << Formatter::Pair.new("name", s.name)
        alist << Formatter::Pair.new("guid", s.guid)
        alist << Formatter::Pair.new("parent", s.parentGuid)
        formatter << alist
      end

      #tag_info.print_tree
      puts formatter.to_s
    end
  end

  #
  # ListNotes Command
  #
  class ListNotes < Command
    def self.get_command_name
      "listnotes"
    end

    def initialize
      super self.class.get_command_name

      @opt.on "-t", "--tag tag_guids", Array do |tag_list|
        @tags = tag_list
      end

      @opt.on "-q", "--query query", String do |query|
        @query = Utils::unpack_utf8_string query
      end
    end

    def exec_impl(args)
      @tags, @query = nil, nil
      parse_args args, 0

      filter = Evernote::EDAM::NoteStore::NoteFilter.new
      filter.order = Evernote::EDAM::Type::NoteSortOrder::UPDATED
      filter.tagGuids = @tags
      filter.words = @query
      notelist = @note_store.findNotes(@auth_token,
                                       filter,
                                       0,
                                       Evernote::EDAM::Limits::EDAM_USER_NOTES_MAX)
      formatter = Formatter.new
      notelist.notes.each do |n|
        formatter << Utils::get_note_formatter(n)
      end
      puts formatter.to_s
    end
  end

  #
  # GetNoteContent Command
  #
  class GetNoteContent < Command
    def self.get_command_name
      "getnotecontent"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} guid"
      @opt.on "-x", "--xhtml" do
        @get_as_xhtml = true
      end
      @opt.on "--text" do
        @get_as_xhtml = false
      end
    end

    def exec_impl(args)
      @get_as_xhtml = false
      guid, = parse_args args, 1

      content = @note_store.getNoteContent @auth_token, guid
      unless @get_as_xhtml
        content =~ %r|<en-note>(.*)</en-note>|m
        content = $1
        content.gsub! %r|<br.*?/>|m, "\n"
        content.gsub! %r|&nbsp;|m, " "
        content = CGI.unescapeHTML content
      end
      print content.to_s # Don't use puts not to include redundant '\n' in the content.
    end
  end

  #
  # CreateNote Command
  #
  class CreateNote < Command
    def self.get_command_name
      "createnote"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} title"

      @opt.on "-t", "--tag tag_names", Array do |tag_list|
        @tags = Utils::unpack_utf8_string_list tag_list
      end

      @opt.on "-x", "--xhtml" do
        @save_as_xhtml = true
      end

      @opt.on "--text" do
        @save_as_xhtml = false
      end

      @opt.on "-c", "--content" do
        @has_content = true
      end
    end

    def exec_impl(args)
      @tags, @save_as_xhtml, @has_content, content = nil, false, false, nil
      title, = parse_args args, 1
      if @has_content
        content = get_text_block
        unless @save_as_xhtml
          content = Utils::to_xhtml content
        end
      end
      note = Evernote::EDAM::Type::Note.new
      note.title = title
      note.tagNames = @tags
      note.content = content if content
      note.attributes = Evernote::EDAM::Type::NoteAttributes.new
      if @save_as_xhtml
        note.attributes.sourceApplication = APPLICATION_NAME_XHTML
      else
        note.attributes.sourceApplication = APPLICATION_NAME_TEXT
      end
      note = @note_store.createNote @auth_token, note

      formatter = Utils::get_note_formatter note
      puts formatter.to_s
    end
  end

  #
  # UpdateNote Command
  #
  class UpdateNote < Command
    def self.get_command_name
      "updatenote"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} guid title"

      @opt.on "-t", "--tag tag_names", Array do |tag_list|
        @tags = Utils::unpack_utf8_string_list tag_list
      end

      @opt.on "--delete-all-tags" do
        @tags = []
      end

      @opt.on "-x", "--xhtml" do
        @save_as_xhtml = true
      end

      @opt.on "--text" do
        @save_as_xhtml = false
      end

      @opt.on "-c", "--content" do
        @has_content = true
      end
    end

    def exec_impl(args)
      @tags, @save_as_xhtml, @has_content, content = nil, false, false, nil
      guid, title = parse_args args, 2
      if @has_content
        content = get_text_block
        unless @save_as_xhtml
          content = Utils::to_xhtml content
        end
      end
      note = Evernote::EDAM::Type::Note.new
      note.guid = guid
      note.title = title
      note.tagNames = @tags
      note.content = content if content
      note.attributes = Evernote::EDAM::Type::NoteAttributes.new
      if @save_as_xhtml
        note.attributes.sourceApplication = APPLICATION_NAME_XHTML
      else
        note.attributes.sourceApplication = APPLICATION_NAME_TEXT
      end
      note = @note_store.updateNote @auth_token, note

      formatter = Utils::get_note_formatter note
      puts formatter.to_s
    end
  end

  #
  # DeleteNote Command
  #
  class DeleteNote < Command
    def self.get_command_name
      "deletenote"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} guid"
    end

    def exec_impl(args)
      guid, = parse_args args, 1
      @note_store.deleteNote @auth_token, guid
    end
  end

  #
  # ExpungeNote Command
  #
  class ExpungeNote < Command
    def self.get_command_name
      "expungenote"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} guid"
    end

    def exec_impl(args)
      guid, = parse_args args, 1
      @note_store.expungeNote @auth_token, guid
    end
  end

  #
  # CreateTag Command
  #
  class CreateTag < Command
    def self.get_command_name
      "createtag"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} name"

      @opt.on "-p", "--parent parent_tag_name", String do |parent_tag_name|
        parent_tag_guid = Utils::unpack_utf8_string parent_tag_name
        parent_tag_guid = @tag_info.get_tag_guid parent_tag_name
        if parent_tag_guid == nil
          raise IllegalArgumentException.new(%|tag "#{parent_tag_name}" is not found|)
        end
        @parent_tag = parent_tag_guid
      end
    end

    def exec_impl(args)
      @tag_info = create_tag_info
      @parent_tag = nil
      name, = parse_args args, 1

      tag = Evernote::EDAM::Type::Tag.new
      tag.name = name
      tag.parentGuid = @parent_tag
      tag = @note_store.createTag @auth_token, tag
      puts %|"#{tag.guid}"|
    end
  end

  #
  # UpdateTag Command
  #
  class UpdateTag < Command
    def self.get_command_name
      "updatetag"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} name"

      @opt.on "-p", "--parent parent_tag_name", String do |parent_tag_name|
        parent_tag_guid = Utils::unpack_utf8_string parent_tag_name
        parent_tag_guid = @tag_info.get_tag_guid parent_tag_name
        if parent_tag_guid == nil
          raise IllegalArgumentException.new(%|tag "#{parent_tag_name}" is not found|)
        end
        @parent_tag = parent_tag_guid
      end

      @opt.on "-r", "--rename new_tag_name", String do |new_tag_name|
        @new_tag_name = Utils::unpack_utf8_string new_tag_name
      end
    end

    def exec_impl(args)
      @tag_info = create_tag_info
      @parent_tag, @new_tag_name = nil, nil
      name, = parse_args args, 1

      tag_guid = @tag_info.get_tag_guid name
      if tag_guid == nil
        raise IllegalArgumentException.new(%|tag "#{name}" is not found|)
      end
      if @new_tag_name == nil
        @new_tag_name = name
      end

      tag = Evernote::EDAM::Type::Tag.new
      tag.guid = tag_guid
      tag.name = @new_tag_name
      tag.parentGuid = @parent_tag
      tag = @note_store.updateTag @auth_token, tag
    end
  end

  #
  # ExpungeTag Command
  #
  class ExpungeTag < Command
    def self.get_command_name
      "expungetag"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} name"
    end

    def exec_impl(args)
      @tag_info = create_tag_info
      name, = parse_args args, 1

      tag_guid = @tag_info.get_tag_guid name
      if tag_guid == nil
        raise IllegalArgumentException.new(%|tag "#{name}" is not found|)
      end

      @note_store.expungeTag @auth_token, tag_guid
    end
  end

  #
  # ListSearch Command
  #
  class ListSearch < Command
    def self.get_command_name
      "listsearch"
    end

    def initialize
      super self.class.get_command_name
    end

    def exec_impl(args)
      parse_args args, 0
      searches = @note_store.listSearches @auth_token
      formatter = Formatter.new
      searches.each do |s|
        formatter << Utils::get_search_formatter(s)
      end
      puts formatter.to_s
    end
  end

  #
  # CreateSearch Command
  #
  class CreateSearch < Command
    def self.get_command_name
      "createsearch"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} name query"
    end

    def exec_impl(args)
      name, query = parse_args args, 2
      search = Evernote::EDAM::Type::SavedSearch.new
      search.name = name
      search.query = query
      search = @note_store.createSearch @auth_token, search

      formatter = Utils::get_search_formatter search
      puts formatter.to_s
    end
  end

  #
  # UpdateSearch Command
  #
  class UpdateSearch < Command
    def self.get_command_name
      "updatesearch"
    end

    def initialize
      super self.class.get_command_name
      @opt.banner = "#{@name} guid name query"
    end

    def exec_impl(args)
      guid, name, query = parse_args args, 3
      search = Evernote::EDAM::Type::SavedSearch.new
      search.guid = guid
      search.name = name
      search.query = query
      @note_store.updateSearch @auth_token, search

      formatter = Utils::get_search_formatter search
      puts formatter.to_s
    end
  end

  #
  # Utils
  #
  class Utils
    def self.to_xhtml(content)
      content = CGI.escapeHTML content
      content.gsub! %r| |, %|&nbsp;|
      content.gsub! %r|\n|, %|<br clear="none"/>|
      content = NOTE_DEFAULT_HEADER + content + NOTE_DEFAULT_FOOTER
    end

    def self.unpack_utf8_string(str)
      if str =~ /"(\\\d\d\d)+"/
        #puts "match"
        utf8_str = eval str
        utf8_str.force_encoding "ASCII-8BIT" if utf8_str.respond_to? :force_encoding
        utf8_str
      else
        str
      end
    end

    def self.unpack_utf8_string_list(str_list)
      str_list.map do |elem|
        unpack_utf8_string elem
      end
    end

    def self.get_note_formatter(note)
      formatter = Formatter.new
      formatter << Formatter::Pair.new("name", note.title)
      formatter << Formatter::Pair.new("guid", note.guid)
      formatter << Formatter::Pair.new("created", Time.at(note.created/1000))
      formatter << Formatter::Pair.new("updated", Time.at(note.updated/1000))

      if note.tagGuids
        tag_list = Formatter.new
        note.tagGuids.each do |tag_guid|
          tag_list << tag_guid
        end
        formatter << Formatter::Pair.new("tags", tag_list)
      end

      formatter << Formatter::Pair.new("edit-mode", get_edit_mode(note.attributes.sourceApplication))
      formatter
    end

    def self.get_search_formatter(search)
      formatter = Formatter.new
      formatter << Formatter::Pair.new("name", search.name)
      formatter << Formatter::Pair.new("guid", search.guid)
      formatter << Formatter::Pair.new("query", search.query)
      formatter
    end

    def self.get_edit_mode(src_app)
      if src_app =~ /\Aemacs-enclient (\{.*\})\z/
        attr = eval $1
        attr[:editmode]
      else
        "XHTML"
      end
    end
  end

  #
  # Command Driver
  #
  class Shell
    def initialize
      @command_classes = []
    end

    def add_command(command_class)
      @command_classes << command_class
    end

    def run
      add_command Login
      add_command ListNotebooks
      add_command ListTags
      add_command ListNotes
      add_command GetNoteContent
      add_command CreateNote
      add_command UpdateNote
      add_command DeleteNote
      add_command ExpungeNote
      add_command CreateTag
      add_command UpdateTag
      add_command ExpungeTag
      add_command ListSearch
      add_command CreateSearch
      add_command UpdateSearch

      command_found = false
      command_name = ARGV[0]
      command_class = @command_classes.find do |v|
        v.get_command_name == command_name
      end

      if command_class
        ARGV.shift
        command = command_class.new
        return command.exec(ARGV)
      elsif command_name == "help"
        help = "Usage: #{$0} <subcommand> [options] [args]\n\n"
        help << "Available subcommands:\n\n"
        @command_classes.each do |comm|
          help << "  #{comm.new.description}\n"
        end
        $stderr.puts help
        return E_OK
      else
        $stderr.puts %|invalid subcommand "#{command_name}"|
        return E_FAIL
      end
    end
  end
end # module EnClient


if __FILE__ == $0


begin
  result = EnClient::Shell.new.run
rescue
  $stderr.puts $!.backtrace
  result = 2 # unexpected error
end

exit result

end


