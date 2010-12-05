Emacs evernote mode

Last Modified: 2010-12-05

License
=======

Copyright 2010 Yusuke Kawakami

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.


Introduction
============

Emacs evernote mode offers functions to refer and edit
Evernote notes directly from Emacs. Currently this package offers the
following commands.


* evernote-open-note
  Read an existing note to an Emacs buffer. Tag search is also
  available. You can input the name and the tag of the note with
  completion in the minibuffer.

* evernote-save-note (default bound to \C-x\C-s)
  Save the modified note.

* evernote-create-note
  Create a note from scratch.

* evernote-write-note (default bound to \C-cee)
  Create a new note from the current buffer.
  You can attach tags to the note when creating the note.

* evernote-post-region
  Create a new note containing the selected region. If you call this
  command without argument, no buffer associated with the new note is
  generated, which is different from evernote-open-note and
  evernote-create-note. If you call this command with an argument by
  \C-u etc.(not default argument), it creates a new buffer associated
  with the new note and you can edit the note in the buffer.

* evernote-edit-tags (default bound to \C-cet)
  Change tags attached to the note. The change on Evernote service
  will be made after exec of evernote-save-note.

* evernote-change-edit-mode (default bound to \C-cee)
  Change the edit mode of the note. (See "Evernote note edit mode" for
  details) The change on Evernote service will be made after exec of
  evernote-save-note.

* evernote-rename-note (default bound to \C-cer)
  Rename the note. The change on Evernote service will be made after
  exec of evernote-save-note.

* evernote-delete-note (default bound to \C-ced)
  Delete the note.

* evernote-search-notes
  Search notes by query in the minibuffer. (See "Search Query Examples" for details)

* evernote-do-saved-search
  Search notes by using a Saved Search.

* evernote-create-search
  Save a query with a name for re-use at a later date.

* evernote-edit-search
  Change the name and the query of an existing Saved Search.

* evernote-browser
  Open Evernote Browser. Evernote Browser offers the features to
  open notes from the tag hierarchical list, the saved search list
  and the note list used before. See "Evernote Browser" for details.


After calling evernote-open-note or evernote-write-note,
they validate the minor-mode "evernote-mode" in the buffer.

The following commands are available only when "evernote-mode" is valid.
- evernote-save-note
- evernote-edit-tags
- evernote-change-edit-mode
- evernote-rename-note
- evernote-delete-note


Evernote note edit mode
=========================

Evernote notes are XML complying with ENML
DTD(http://xml.evernote.com/pub/enml2.dtd).
evernote-mode offers the two edit mode, XHTML mode and TEXT mode for
saving and reading the Evenote notes.

When you save the Evernote note, the content of the buffer will be
saved as the content of the note. Therefore if the contents of the
buffer is not valid ENML, you cannot save the contents because of the
error. And when you read a Evernote note by using XHTML mode, the XML
document is read "as is" to the Emacs buffer.


An example of XHTML mode editing:

   Emacs Buffer
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>Evernote notes are XML complying with ENML DTD<br clear="none"/>
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for<br clear="none"/>
   saving and reading the Evenote notes.<br clear="none"/>
   </en-note>
   -----------------------------------
   |
   | save on XHTML mode
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
   |
   | read on XHTML mode
   V
   Emacs Buffer (same as the content of the note)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>Evernote notes are XML complying with ENML DTD<br clear="none"/>
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for<br clear="none"/>
   saving and reading the Evenote notes.<br clear="none"/>
   </en-note>
   -----------------------------------


The text mode is specialized in editing Evernote notes that only
contain text content.
When you save an Evernote note, special characters(&keyword; space,
newline) in the buffer are escaped and the root element is added
to transform it into a XML document. Therefore text contents in the
Emacs buffer is same as the appearance of the note. (is not same as
the actual XML content of the note.)
And when you read a Evernote note by using TEXT mode, evernote-mode
reads the contents under the root element of the XML as a text and all
the special characters are unescaped then evernote-mode insert it to
the buffer.


An example of TEXT mode editing:

   Emacs Buffer
   -----------------------------------
   Evernote notes are XML complying with ENML DTD
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for
   saving and reading the Evenote notes.
   -----------------------------------
   |
   | save on TEXT mode
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
   |
   | read on TEXT mode
   V
   Emacs Buffer (unescaped and transformed int a text)
   -----------------------------------
   Evernote notes are XML complying with ENML DTD
   evernote-mode offers the two edit mode, XHTML mode and TEXT mode for
   saving and reading the Evenote notes.
   -----------------------------------


You can select the edit mode when you create a note or when you call
evernote-change-edit-mode. The edit mode is recorded in the note when
you save it, and the recorded edit mode is applied to the note when you reopen it.
Notes created by other Evernote applications are read as XHTML mode,
but you can change the edit-mode by evernote-change-edit-mode.

evernote-mode doesn't offer any rich function for editing XML, so it
is not appropriate for editing notes created other Evernote
applications.
The main usage of evernote-mode is to create and edit notes as TEXT
mode.


Search Query Examples
=====================

The following examples are referred from
(http://www.evernote.com/about/developer/api/evernote-api.htm#_Toc277181479).

* Find notes containing the word "chicken", tagged with "cooking", and created this year:

  chicken tag:cooking created:year


* Find notes tagged with "cooking" but not "mexican" that include the word "beef" but not the word "carrots"

  tag:cooking -tag:mexican beef -carrots


* Find notes in my "Travel" notebook with San Francisco in the title:

  notebook:Travel intitle:"San Francisco"


* Find notes that either include the text "San Francisco" or are tagged with the "SFO" tag:

  any: "San Francisco" tag:SFO


Evernote Browser
=========================

Evernote Browser offers the features to open notes from the tag
hierarchical list, the saved search list, and the note lists used
before. These lists are always kept in Emacs buffers after they are
created, and they make the procedure to open notes easy.

Evernote Browser is composed of multiple Evernote Browser pages(emacs
bufferes). An Evernote Browser page is created when searching notes
first or when executing the evernote-browser command at the state that
no search has been executed. The multiple pages are managed as the
page list and each page has the next/previous page.
There is one valid current page. Execute the evernote-browser command
to move the cursor to the current Evernote Browser page. Also, use the
key for moving to the next/previous page key (described later) to move
to another page.


There are three type of Evernote Browser page

- tag list page
- saved search list page
- note list page

A tag list page shows the hierarchical tag list created on Evernote
service. Pressing [Enter](\C-m) on the tag name opens a note list page
of the tag.

A saved search list shows the search list created on Evernote
service. Pressing [Enter](\C-m) on the search name opens a note list
page from the search result.

A note list shows the note list from a note search result. The note
list page is newly created by the evenote-open-note command, the
evernote-search-notes command and by searches on Evernote Browser.
Pressing [Enter](\C-m) on the note name opens a note.


The followings are other key assignments on Evernote Browser pages

b: show the previous page
f: show the next page
t: create a tag list page and show it. If a tag list page already
   exists, move the cursor to the page
S: create a search list page and show it. If a tag list page already
   exists, move the cursor to the page
s: create a note list from the search query input and show it.
o: same as [Enter](\C-m), but it does not move the cursor to the
   opened note.
d: delete the current from Evernote Browser


Installation & Settings
=======================

1. Install ruby scripts for using Evernote service.

cd evernote-mode/ruby
ruby setup.rb


2. Copy evernote-mode.el to your load path.

cp evernote-mode.el <your load path>


3. Add the evernote-mode configuration to .emacs.

(add-to-list 'load-path "<your load path>")
(require 'evernote-mode)
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)


4. Proxy settings

If you want to use the proxy, set the value to the environment
variable 'EN_PROXY' written as 'host:port' format.
(ex. export EN_PROXY=proxy.hoge.com:8080)

