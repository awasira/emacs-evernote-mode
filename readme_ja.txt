Emacs evernote mode

Last Modified: 2010-11-6

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

Emacs evernote modeはEvernoteのノートをemacsから直接参照、編集するため
の機能を提供します。現在このパッケージでは以下のコマンドを提供して
います。


* evernote-open-note
  既存のノートをemacsバッファに読み込みます.タグを指定してノートの絞込
  み検索が可能です.タグ・ノート名指定の際には、ミニバッファでの補完が
  行われます.

* evernote-save-note (default bound to \C-x\C-s)
  編集したノートをEvernoteサービス上で上書き保存します。

* evernote-create-note
  ノートを新規作成します。

* evernote-write-note (default bound to \C-cee)
  emacsバッファを新規ノートとして保存します.保存時にはノートに付加する
  タグを指定できます。

* evernote-edit-tags (default bound to \C-cet)
  ノートに付加するタグを変更します.このコマンド発行後に
  evernote-save-noteを実行することでEvernoteサービス上で変更が反映され
  ます。

* evernote-change-edit-mode (default bound to \C-cee)
  ノートの編集モードを変更します.詳細は "Evernote note editformat" を
  参照して下さい.このコマンド発行後にevernote-save-noteを実行すること
  でEvernoteサービス上で変更が反映されます。

* evernote-rename-note (default bound to \C-cer)
  ノートを名前を変更します.このコマンド発行後にevernote-save-noteを実
  行することでEvernoteサービス上で変更が反映されます。

* evernote-delete-note (default bound to \C-ced)
  ノートを削除します。

* evernote-search-notes
  ミニバッファから入力されたクエリを使ってノートを検索します。
  クエリの例は "Search Query Examples" を参照して下さい。

* evernote-create-saved-search
  Evernoteの「保存された検索」を使ってノートを検索します。

* evernote-create-saved-search
  クエリに名前をつけて保存し、以後「保存された検索」として参照できるようにします。


evernote-open-note, evernote-write-noteを実行すると、ノートが読み込ま
れているバッファで evernote-modeマイナーモードが起動されます。


以下のコマンドはevernote-modeマイナーモードでのみ有効です。
- evernote-save-note
- evernote-edit-tags
- evernote-change-edit-mode
- evernote-rename-note
- evernote-delete-note


Evernote note edit mode
=========================

EvernoteのノートはENML DTD(http://xml.evernote.com/pub/enml2.dtd)に準
拠するXML文書です。evernote-modeではこのXMLをemacsで保存、読み込みする
為にXHTMLモードとTEXTモード2種類の編集モードを用意しています。

XHTMLモードでノートを保存した場合、バッファの内容がそのままノートの内
容と保存されます。バッファ内容がENML DTDに沿ったフォーマットでない場合
はエラーになります。また、XHTMLモードでノートを読み込んだ場合、XML文書
はそのまま加工されずバッファに読み込まれます。


XHTMLモードでの編集の例:

   Emacs バッファ
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>EvernoteのノートはENML DTDに準<br clear="none"/>
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー<br clear="none"/>
   ドとTEXTモード2種類の編集モードを用意しています。<br clear="none"/>
   </en-note>
   -----------------------------------
   |
   |XHTMLモードで保存
   V
   Evernoteサービス上のノート(Emacsバッファの内容と同じ)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>EvernoteのノートはENML DTDに準<br clear="none"/>
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー<br clear="none"/>
   ドとTEXTモード2種類の編集モードを用意しています。<br clear="none"/>
   </en-note>
   -----------------------------------
   |
   |XHTMLモードで読み込み
   V
   Emacs バッファ(ノートの内容と同じ)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>EvernoteのノートはENML DTDに準<br clear="none"/>
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー<br clear="none"/>
   ドとTEXTモード2種類の編集モードを用意しています。<br clear="none"/>
   </en-note>
   -----------------------------------


TEXTモードはテキストのみ含むEvernoteノートの編集に特化したモードです。
TEXTモードでノートを保存した場合、バッファ中のXMLの特殊文字(&キーワー
ド;, スペース、改行)はエスケープされ、ルート要素を付加した上でENMLに変
換されます。このため、emacsバッファで表示されている内容がノートの見た
目上の内容として保存されます。また、TEXTモードでノートを読み込んだ場合
は、XMLのルート要素直下をテキストとして解釈し、XMLの特殊文字はアンエス
ケープされた上でバッファに読み込まれます。


TEXTモードでの編集の例:

   Emacs バッファ
   -----------------------------------
   EvernoteのノートはENML DTDに準
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー
   ドとTEXTモード2種類の編集モードを用意しています。
   -----------------------------------
   |
   |TEXTモードで保存
   V
   Evernoteサービス上のノート
   (Emacsバッファの内容がエスケープされ, XMLに変換される)
   -----------------------------------
   <?xml version="1.0" encoding="UTF-8"?>
   <!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
   <en-note>EvernoteのノートはENML DTDに準<br clear="none"/>
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー<br clear="none"/>
   ドとTEXTモード2種類の編集モードを用意しています。<br clear="none"/>
   </en-note>
   -----------------------------------
   |
   |TEXTモードで読み込み
   V
   Emacs バッファ
   (ノートのルート要素以下の内容がアンエスケープされる)
   -----------------------------------
   EvernoteのノートはENML DTDに準
   拠するXML文書です。evernote-modeではこのXMLをemacsで扱うためにXHTMLモー
   ドとTEXTモード2種類の編集モードを用意しています。
   -----------------------------------


上記XHTML, TEXTモードはノートの作成時、もしくは
evernote-change-edit-modeで選択できます。ノートには編集モード情報が保
存され、次に読み込まれる際には保存時の編集モードで読み込まれます。また、
他のEvernoteアプリケーションで作成されたノートはXHTMLモードとして読み
込まれます。

evernote-modeにはXML編集機能が備わっていないので、他のEvernoteアプリケー
ションで作成されたノートの編集には適していません。evernote-modeでEmacs
バッファをTEXTモードでそのまま保存、編集することが主な用途となります。


Search Query Examples
=====================

以下の例は
http://www.evernote.com/about/developer/api/evernote-api.htm#_Toc277181479
からの引用です。

* Find notes containing the word "chicken", tagged with "cooking", and created this year:

  chicken tag:cooking created:year


* Find notes tagged with "cooking" but not "mexican" that include the word "beef" but not the word "carrots"

  tag:cooking -tag:mexican beef ?carrots


* Find notes in my "Travel" notebook with San Francisco in the title:

  notebook:Travel intitle:"San Francisco"


* Find notes that either include the text "San Francisco" or are tagged with the "SFO" tag:

  any: "San Francisco" tag:SFO


* Find image notes from the Sunnyvale region:

  resource:image/* latitude:37 -latitude:38
  longitude:-123 -longitude:-122


* Find untagged audio notes that I edited in the last week or two:

  -tag:* resource:audio/* updated:week-1


Installation & Settings
=======================

1. Evernoteサービス使用のために必要なRubyスクリプトをインストールします。

cd evernote-mode/ruby
ruby setup.rb


2. evernote-mode.el をロードパスにコピーする

cp evernote-mode.el <your load path>


3. evernote-mode設定を.emacs に追記

(add-to-list 'load-path "<your load path>")
(require 'evernote-mode)
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)


4. proxy の設定

プロキシを使用する場合は環境変数EN_PROXYに 'プロキシホスト':'ポート'
を指定して下さい。(ex. export EN_PROXY=proxy.hoge.com:8080)
