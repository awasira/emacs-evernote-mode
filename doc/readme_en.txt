                         Emacs evernote mode
                         ===================

Author: Yusuke Kawakami <Yusuke Kawakami>
Date: 2011-06-24 06:49:58 JST


Table of Contents
=================
1  License 
2 Introduction 
3 Evernote note edit mode 
    3.1 XHTML mode 
        3.1.1  An example of XHTML mode editing: 
    3.2 TEXT mode 
        3.2.1  An example of TEXT mode editing: 
    3.3 Select edit mode 
    3.4 Change edit mode 
4 Search Query Examples 
5 Evernote Browser 
6 Bookmarks 
7 Install and Settings 
8 Collaboration with Anything 
9 Troubleshooting 
    9.1 `require': no such file to load -- gdbm 
    9.2 `require': no such file to load -- net/https 
    9.3 No such file or directory -- enclient.rb (LoadError) 


1 QUOTE License 
~~~~~~~~~~~~~~~~

Copyright 2011 Yusuke Kawakami

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.


2 Introduction 
~~~~~~~~~~~~~~~

Emacs evernote mode offers functions to refer and edit Evernote notes directly from Emacs. Currently this package offers the following interfaces.

  - *Command: evernote-login*

    Login to Evernote. The following commands are available only when you login.
    If you execute the following commands without the login, the login prompt will be shown.

  - *Variable: evernote-username*

    An username of your evernote.

  - *Command: evernote-open-note*

    Read an existing note to an Emacs buffer. At first, input tags of the notes, then input the name of the note on the minibuffer.

  - *Command: evernote-open-note-in-notebook*

    Read an existing note to an Emacs buffer. At first, input the notebook and tags of the notes, then input the name of the note on the minibuffer.

  - *Command: evernote-save-note (default bound to \C-x\C-s)*

    Save the modified note..

  - *Command: evernote-create-note*

    Create a note from scratch. The note is created in the default notebook.

  - *Command: evernote-create-note-in-notebook*

    Create a note from scratch. The note is created in the specified notebook.

  - *Command: evernote-write-note*

    Create a new note from the current buffer. The note is created in the default notebook.

  - *Command: evernote-write-note-in-notebook*

    Create a new note from the current buffer. The note is created in the specified notebook.

  - *Command: evernote-post-region*

    Create a new note containing the selected region. The note is created in the default notebook. If you call this command without argument, no buffer associated with the new note is generated, which is different from evernote-open-note and evernote-create-note. If you call this command with an argument by \C-u etc.(not default argument), it creates a new buffer associated with the new note and you can edit the note in the buffer.

  - *Command: evernote-post-region-in-notebook*

    Create a new note containing the selected region. The note is created in the specified notebook.

  - *Command: evernote-change-notebook (default bound to \C-cen)*

    Change the notebook to which the note belongs. The change on Evernote service will be made after exec of evernote-save-note.

  - *Command: evernote-edit-tags (default bound to \C-cet)*

    Change tags attached to the note. The change on Evernote service will be made after exec of evernote-save-note.

  - *Command: evernote-change-edit-mode (default bound to \C-cee)*

    Change the edit mode of the note. (See [Evernote note edit mode] for details) The change on Evernote service will be made after exec of evernote-save-note.

  - *Command: evernote-rename-note (default bound to \C-cer)*

    Rename the note. The change on Evernote service will be made after exec of evernote-save-note.

  - *Command: evernote-delete-note (default bound to \C-ced)*

    Delete the note.

  - *Command: evernote-search-notes*

    Search notes by query in the minibuffer. (See [Search Query Examples] for details)

  - *Command: evernote-do-saved-search*

    Search notes by using a Saved Search.

  - *Command: evernote-create-notebook*

    Create a new notebook.

  - *Command: evernote-edit-notebook*

    Change the name and whether this book is the default notebook or not.

  - *Command: evernote-create-search*

    Save a query with a name for later use.

  - *Command: evernote-edit-search*

    Change the name and the query of an existing Saved Search.

  - *Command: evernote-toggle-read-only (default bound to \C-x\C-q)*

    Toggle read-only status of the note. When a note of XHTML mode is changed to the read-only status, the command in the variable evernote-enml-formatter-command formats the XHTML. See [Evernote note edit mode] for details.

  - *Variable: evernote-enml-formatter-command*

    The command to format XHTML. When a note of XHTML mode is read-only, the command formats the XHTML and displays it. Currently evernote-mode supports w3m as the formatter.

  - *Command: evernote-browser*

    Open Evernote Browser. Evernote Browser offers the features to open notes from the tag hierarchical list, the saved search list and the note list of the past search result. See [Evernote Browser] for details.

  - *Variable: anything-c-source-evernote-title*

    The variable that offers the function for Anything([http://www.emacswiki.org/emacs/Anything]) to display the note candidates from the title.
    See [Collaboration with Anything] for details.

  - *Command: anything-evernote-title*

    Open a note by using Anything. See [Collaboration with Anything] for details.

  - *Variable: evernote-mode-display-menu*

    Display the menu on the menubar for evernote-mode (default: t)

  - *Variable: evernote-password-cache*

    Non-nil means that password cache is enabled.
    It is recommended to encrypt the file with EasyPG([http://epg.sourceforge.jp/]). EasyPG is included in Emacs 23 or later. On Emacs 22, you install the EasyPG, then add the following line to your .emacs
    
    (require 'epa-setup)
    
    You can cache the password safely by using gpg-agent.

When you create a note by evernote-create-note, evernote-write-note and evernote-post-region, you can attach tags to the note.
Also, when using the commands, you can input the name and the tag of the note with completion in the minibuffer.


The minor-mode "evernote-mode" is applied to the buffers opening the evernote note. The following commands are available only when "evernote-mode" is valid.

  - evernote-save-note
  - evernote-change-notebook
  - evernote-edit-tags
  - evernote-change-edit-mode
  - evernote-rename-note
  - evernote-delete-note



  [Evernote note edit mode]: sec-3
  [Search Query Examples]: sec-4
  [Evernote note edit mode]: sec-3
  [Evernote Browser]: sec-5
  [Collaboration with Anything]: sec-8
  [Collaboration with Anything]: sec-8

3 Evernote note edit mode 
~~~~~~~~~~~~~~~~~~~~~~~~~~

Evernote notes are XML complying with ENML DTD([http://xml.evernote.com/pub/enml2.dtd]). evernote-mode offers the two edit mode, XHTML mode and TEXT mode for saving and reading the Evenote notes.


3.1 XHTML mode 
===============

When you save the Evernote note in XHTML mode, the content of the buffer will be saved as the content of the note. Therefore if the contents of the buffer is not valid ENML, you cannot save the contents because of the error.

And when you read a Evernote note by using XHTML mode, the buffer is read-only as the initial state. If the variable evernote-enml-formatter-command is set, the content of the buffer is formatted. (See [Install and Settings] for details of setting evernote-enml-formatter-command) If you toggle the read-only status and change it to read-write, unformatted XHTML is displayed in the buffer. The buffer will display the formatted text if you change the state to read-only again.



[Install and Settings]: sec-7

3.1.1 QUOTE An example of XHTML mode editing: 
----------------------------------------------

   Emacs Buffer
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>Evernote notes are XML complying with ENML DTD<br clear="none"/>
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for<br clear="none"/>
   saving and reading the Evenote notes.<br clear="none"/>
   </en-note>
   -----------------------------------
     save on XHTML mode
   V
   The content of the note (same as the emacs buffer)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>Evernote notes are XML complying with ENML DTD<br clear="none"/>
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for<br clear="none"/>
   saving and reading the Evenote notes.<br clear="none"/>
   </en-note>
   -----------------------------------
     read on XHTML mode
   V
   Emacs Buffer (read-only and formatted)
   -----------------------------------
   Evernote notes are XML complying with ENML DTD
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for
   saving and reading the Evenote notes.
   -----------------------------------
    toggle read-only(evernote-toggle-read-only: \C-x\C-q)
   V
   Emacs Buffer (unformatted)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>Evernote notes are XML complying with ENML DTD<br clear="none"/>
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for<br clear="none"/>
   saving and reading the Evenote notes.<br clear="none"/>
   </en-note>
   -----------------------------------


3.2 TEXT mode 
==============

In XHTML mode, you have to edit a XHTML as a text but it is a complicated work. evernote-mode offers TEXT mode to make editing text notes easy and effective.

The text mode is specialized in editing Evernote notes that only contain text content. When you save an Evernote note, special characters(&keyword; space, newline) in the buffer are escaped and the root element is added to transform it into a XML document. Therefore text contents in the Emacs buffer is same as the appearance of the note. (is not same as the actual XML content of the note.) And when you read a Evernote note by using TEXT mode, evernote-mode reads the contents under the root element of the XML as a text and all the special characters are unescaped then evernote-mode insert it to the buffer.


3.2.1 QUOTE An example of TEXT mode editing: 
---------------------------------------------

Emacs Buffer
   -----------------------------------
   Evernote notes are XML complying with ENML DTD
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for
   saving and reading the Evenote notes.
   -----------------------------------
     save on TEXT mode
   V
   The content of the note (escaped and transformed into a XML)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>Evernote notes are XML complying with ENML DTD<br clear="none"/>
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for<br clear="none"/>
   saving and reading the Evenote notes.<br clear="none"/>
   </en-note>
   -----------------------------------
     read on TEXT mode
   V
   Emacs Buffer (unescaped and transformed int a text)
   -----------------------------------
   Evernote notes are XML complying with ENML DTD
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for
   saving and reading the Evenote notes.
   -----------------------------------

3.3 Select edit mode 
=====================

You can select the edit mode when you create a note. The edit mode is recorded in the note when you save it, and the recorded edit mode is applied to the note when you reopen it.


3.4 Change edit mode 
=====================

Use the command evernote-change-edit-mode to change the edit mode of the existing note. If you change the edit mode from XHTML mode to TEXT mode and the buffer is read-only, then the content of the TEXT mode is the formatted text. Note that the change will remove all format information(xml tags). If the buffer is read-write, the content of the TEXT mode is the original unformatted text.


4 Search Query Examples 
~~~~~~~~~~~~~~~~~~~~~~~~

Here are examples of queries used for searching notes.

The following examples are referred from ([http://www.evernote.com/about/developer/api/evernote-api.htm#\_Toc277181479]).

    * Find notes containing the word "chicken", tagged with "cooking", and created this year:

    chicken tag:cooking created:year

    * Find notes tagged with "cooking" but not "mexican" that include the word "beef" but not the word "carrots"

    tag:cooking -tag:mexican beef -carrots

    * Find notes in my "Travel" notebook with San Francisco in the title:

    notebook:Travel intitle:"San Francisco"

    * Find notes that either include the text "San Francisco" or are tagged with the "SFO" tag:

    any: "San Francisco" tag:SFO



    [http://www.evernote.com/about/developer/api/evernote-api.htm#\_Toc277181479]: http://www.evernote.com/about/developer/api/evernote-api.htm#_Toc277181479

5 Evernote Browser 
~~~~~~~~~~~~~~~~~~~

Evernote Browser offers the features to open notes from the tag hierarchical list, the saved search list, and the note list of the past search result. These lists are different from the completion lists of evernote-open-note command or the evernote-search-notes command. These lists of Evernote Browser are always kept in Emacs buffers until they are deleted by the user after they were created, and they make the procedure to open notes easy by using these list repeatedly.

Evernote Browser is composed of multiple Evernote Browser pages(emacs buffers). An Evernote Browser page is created when searching notes or when executing the evernote-browser command at the state that no search has been executed. The multiple pages are managed as the page list. There is one valid current page and each page has the next/previous page. Execute the evernote-browser command to move the cursor to the current Evernote Browser page. Also, use the keys for moving to the next/previous page key (described later) to move to another page.

There are three type of Evernote Browser page

    * tag list page
    * saved search list page
    * note list page
    * notebook list page

A tag list page shows the hierarchical tag list created on Evernote service. Pressing Enter(\C-m) on the tag name opens a note list page of the tag.

A saved search list shows the search list created on Evernote service. Pressing Enter(\C-m) on the search name opens a note list page from the search result.

A note list shows the note list from a note search result. The note list page is newly created by the evenote-open-note command, the evernote-search-notes command and by searches on Evernote Browser. Pressing Enter(\C-m) on the note name opens a note.

A notebook list page shows the notebook list created on Evernote service. Pressing Enter(\C-m) on the notebook name opens a note list page associated with the notebook.

The followings are other key assignments on Evernote Browser pages
  Key   Action                                                                                                        
 -----+--------------------------------------------------------------------------------------------------------------
  b     move to the previous page                                                                                     
  f     move to the next page                                                                                         
  t     create a tag list page and show it. If a tag list page already exists, move the cursor to the page            
  S     create a search list page and show it. If a tag list page already exists, move the cursor to the page         
  s     create a note list from the search query input and show it                                                    
  N     create a notebook list page and show it. If a notebook list page already exists, move the cursor to the page  
  o     same as Enter(\C-m), but it does not move the cursor to the opened note                                       
  n     move to the next line and open the note on the cursor if in the note list,                                    
  p     move to the previous line and open the note on the cursor if in the note list,                                
  d     delete the current from Evernote Browser                                                                      


6 Bookmarks 
~~~~~~~~~~~~

On Emacs 23.1 or later, you can set emacs bookmarks to the evernote notes.
You can open immediately the note that you can refer frequently by using this feature.

You can use the bookmarks in the same way as when using bookmarks to regular files.
You can set a bookmark to a evernote note by executing 'bookmark-set' (C-x r m RET) in the buffer opening the evernote note.
Also, you can refer the bookmarks 'bookmark-jump' (C-x r b bookmark RET) or 'list-bookmark' (C-x r l).

7 Install and Settings 
~~~~~~~~~~~~~~~~~~~~~~~

  1. Install ruby scripts for using Evernote service.

    
    cd evernote-mode/ruby
    ruby setup.rb
    

  2. Copy evernote-mode.el to your load path.

    
    cp evernote-mode.el <your load path>
    

  3. Get w3m for evernote-enml-formatter-command (Optional)

     - If you use Linux/Unix, get w3m from [here] and install it or install w3m from the package of your distribution.
     - If you use Win, get cygwin from [here], execute setup.exe and select w3m from the "Select Packages"
     - Add the path of w3m to the environment variable "PATH".

  4. Add the evernote-mode configuration to .emacs.

     
     (add-to-list 'load-path "<your load path>")
     (require 'evernote-mode)
     (setq evernote-username "<your evernote user name>") ; optional: you can use this username as default.
     (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
     (global-set-key "\C-cec" 'evernote-create-note)
     (global-set-key "\C-ceo" 'evernote-open-note)
     (global-set-key "\C-ces" 'evernote-search-notes)
     (global-set-key "\C-ceS" 'evernote-do-saved-search)
     (global-set-key "\C-cew" 'evernote-write-note)
     (global-set-key "\C-cep" 'evernote-post-region)
     (global-set-key "\C-ceb" 'evernote-browser)
     

     If you don't set evernote-enml-formatter-command, the unformatted XHTML is displayed when reading the note of XHTML mode.

                 If you use cygwin for ruby, cygwin-mount.el ([http://www.emacswiki.org/cgi-bin/wiki/cygwin-mount.el]) is also needed.  Get cygwin-mount.el and put it in your load path, and add the followings lines to your .emacs.

     
                 (require 'cygwin-mount)
                 (cygwin-mount-activate)
     

  5. Proxy settings

     If you want to use the proxy, set the value to the environment variable 'EN\_PROXY' written as 'host:port' format. (ex. export EN\_PROXY=proxy.hoge.com:8080)



     [here]: http://w3m.sourceforge.net/index.en.html
     [here]: http://www.cygwin.com/

8 Collaboration with Anything 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

evernote-mode offers the function for Anything([http://www.emacswiki.org/emacs/Anything]) to display the note candidates from the title.
You can add the evernote note titles to the source of candidates of Anything by adding the following configuration to your .emacs.


(add-to-list 'anything-sources anything-c-source-evernote-title)


You can also use 'anything-evernote-title' to choose the candidates only from the evernote notes.
The above features for Anything are available only when you login the evernote service
(after executing evernote-login or the first time you use the command evernote-xxx).

9 Troubleshooting 
~~~~~~~~~~~~~~~~~~

9.1 `require': no such file to load -- gdbm 
============================================

Some distributions do not have the GDBM library for ruby. Install libgdbm-ruby for using GDBM.

- ex. for apt,


apt-get install libgdbm-ruby


- In the case of ActiveScriptRuby or Ruby-mswin32

Retrieve gdbm.dll from [Porting Libraries to Win32], then copy it to the folder where ruby.exe exists.

- In the case of Mac OS X

The pre-installed ruby on Mac OS X does not contain GDBM bindings.
Re-install ruby and GDBM by [MacPorts] or [Homebrew].

MacPorts:

$ sudo ports install ruby gdbm


Homebrew:

$ sudo brew install ruby gdbm


Also, [specify evernote-ruby-command].


[Porting Libraries to Win32]: http://jarp.does.notwork.org/win32/
[MacPorts]: http://www.macports.org/
[Homebrew]: http://mxcl.github.com/homebrew/
[specify evernote-ruby-command]: sec-9.3

9.2 `require': no such file to load -- net/https 
=================================================

Some distributions do not have the openssl library for ruby. Install libopenssl-ruby for using https.

- ex. for apt,


apt-get install libopenssl-ruby

9.3 No such file or directory -- enclient.rb (LoadError) 
=========================================================

When multiple version of ruby are installed in the OS, the evernote-mode may use another ruby that has installed the evernote-mode (The ruby that has executed 'ruby setup.rb').
Specify the full path of ruby to the evernote-ruby-command, before load the evernote-mode.

e.g.
    
    (setq evernote-ruby-command "/your/path/to/ruby")
    (require 'evernote-mode)
    
