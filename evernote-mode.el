;;; evernote-mode.el ---

;;
;;  Copyright 2010 Yusuke Kawakami
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;;
;; evernote-mode home page is at:
;; Author: Yusuke Kawakami
;; Version: 0.03
;; Keywords: tools

;; This emacs lisp offers the interactive functions to open, edit, and update notes of Evernote.
;; The minor mode Evernote-mode is applied to the buffer editing a note of Evernote.
;;
;; Please copy this file into emacs lisp library directory or place it in
;; a directory (for example "~/lisp") and write $HOME/.emacs like this.
;;
;;      (add-to-list 'load-path "~/lisp")
;;      (require 'evernote-mode)
;;      (global-set-key "\C-cec" 'evernote-create-note)
;;      (global-set-key "\C-ceo" 'evernote-open-note)
;;      (global-set-key "\C-cew" 'evernote-write-note)
;;
;; There is one hooks, evernotes-mode-hook.
;; The usage of the hook is shown as follows.
;;
;; (setq evernote-mode-hook
;;   '(lambda ()
;;      (...)))

;;; Code

(defvar evernote-mode nil
  "Non-nil if Evernote mode is enabled.")
(make-variable-buffer-local 'evernote-mode)

(defvar evernote-note-guid nil
  "Note guid of the buffer")
(make-variable-buffer-local 'evernote-note-guid)

(defvar evernote-note-name nil
  "Note name of the buffer")
(make-variable-buffer-local 'evernote-note-name)

(defvar evernote-note-tags nil
  "Tags of the buffer")
(make-variable-buffer-local 'evernote-note-tags)

(defvar evernote-note-edit-mode nil
  "Edit mode of the buffer")
(make-variable-buffer-local 'evernote-note-edit-mode)

(defvar evernote-mode-map (make-sparse-keymap)
  "Keymap used in evernote mode.")

(define-key evernote-mode-map "\C-x\C-s" 'evernote-save-note)
(define-key evernote-mode-map "\C-cet"   'evernote-edit-tags)
(define-key evernote-mode-map "\C-cee"   'evernote-change-edit-mode)
(define-key evernote-mode-map "\C-cer"   'evernote-rename-note)
(define-key evernote-mode-map "\C-ced"   'evernote-delete-note)

(defvar evernote-read-note-name-map
	(copy-keymap minibuffer-local-completion-map))

(define-key evernote-read-note-name-map [tab] 'evernote-note-completion)
(define-key evernote-read-note-name-map "\C-i" 'evernote-note-completion)


;;(defvar evernote-browsing-mode-map (copy-keymap global-map)
;;  "Keymap used in evernote browsing mode.")
;;(define-key evernote-browsing-mode-map "l" 'evernote-command-login)
;;(define-key evernote-browsing-mode-map "t" 'evernote-command-listtags)
;;(define-key evernote-browsing-mode-map "\C-m" 'evernote-open-guid)

(defconst evernote-client-buffer-name "*Evernote*")
(defconst evernote-client-output-buffer-name "*Evernote-Client-Output*")

(defconst evernote-error-ok                  0)
(defconst evernote-error-fail                100)
(defconst evernote-error-parse               101)
(defconst evernote-error-unknown             1)
(defconst evernote-error-bad-data-format     2)
(defconst evernote-error-permission-denied   3)
(defconst evernote-error-internal-error      4)
(defconst evernote-error-data-required       5)
(defconst evernote-error-limit-reached       6)
(defconst evernote-error-quota-reached       7)
(defconst evernote-error-invalid-auth        8)
(defconst evernote-error-auth-expired        9)
(defconst evernote-error-data-conflict      10)
(defconst evernote-error-enml-validation    11)
(defconst evernote-error-shared-unavailable 12)


;;
;; Interactive functions.
;;


(defun evernote-open-note ()
  "Open a note"
  (interactive)
  (let ((open-note-func
         (lambda ()
           (evernote-open-note-common
            (evernote-command-get-note-list-from-tags
             (evernote-completing-read-multiple "Tags used for search (comma separated form. default search all tags):"
                                                (evernote-get-tag-cands (evernote-command-get-tag-list))
                                                nil t))))))
    (evernote-command-with-auth open-note-func)))


(defun evernote-search-notes ()
  "Search notes with query and open a note among them"
  (interactive)
  (let ((search-note-func
         (lambda ()
           (evernote-open-note-common (evernote-command-get-note-list-from-query (read-string "Query:"))))))
    (evernote-command-with-auth search-note-func)))


(defun evernote-do-saved-search ()
  "Do a saved search and open a note"
  (interactive)
  (let ((search-note-func
         (lambda ()
           (evernote-open-note-common
						(evernote-command-get-note-list-from-query
						 (let ((search-cands
										(evernote-get-search-cands (evernote-command-get-search-list))))
							 (cdr (assoc 'query
													 (cdr (assoc
																 (completing-read "Saved search:"
																									search-cands
																									nil t)
																 search-cands))))))))))
    (evernote-command-with-auth search-note-func)))


(defun evernote-open-note-common (note-list)
  "Common procedure of opening a note"
  (let (note-attr note-guid note-name note-edit-mode note-tags opened-buf)
    ;;(setq note-cand (evernote-get-note-cands note-list))
    ;;(setq note-attr (cdr (assoc (completing-read "Note:" note-cand nil t) note-cand)))
		(setq note-attr (evernote-read-note-attr note-list))
    (setq note-guid (cdr (assoc 'guid note-attr)))
    (setq note-name (cdr (assoc 'name note-attr)))
    (setq note-edit-mode (cdr (assoc 'edit-mode note-attr)))
    (setq note-tags (cdr (assoc 'tags note-attr)))
    (setq opened-buf (evernote-find-opened-buffer note-guid))
    (if opened-buf
        (evernote-move-cursor-to-window opened-buf)
      (let ((content (evernote-command-get-note-content note-guid note-edit-mode)))
        (evernote-create-note-buffer note-guid note-name note-edit-mode note-tags content)))))


(defun evernote-save-note ()
  "Save a note"
  (interactive)
  (if (and evernote-note-guid (buffer-modified-p))
      (let ((save-note-func
             (lambda ()
               (progn (evernote-command-update-note (current-buffer)
                                                    evernote-note-guid
                                                    evernote-note-name
                                                    evernote-note-tags
                                                    evernote-note-edit-mode)
                      (set-buffer-modified-p nil)))))
        (evernote-command-with-auth save-note-func))
    (message "(No changes need to be saved)")))


(defun evernote-create-note ()
  "Create a note"
  (interactive)
  (let ((create-note-func
         (lambda ()
           (let* ((tags (evernote-completing-read-multiple "Attached Tags (comma separated form):"
                                                           (evernote-get-tag-cands (evernote-command-get-tag-list))))
                  (name (read-string "Note name:"))
                  (edit-mode "TEXT")
                  (buf (generate-new-buffer name)))
             (switch-to-buffer buf)
             (evernote-command-create-note (current-buffer)
                                           name
                                           tags
                                           edit-mode)
             (setq evernote-note-guid (evernote-eval-command-result)
                   evernote-note-name name
                   evernote-note-tags tags
                   evernote-note-edit-mode edit-mode)
             (evernote-mode)
             (evernote-update-mode-line)
             (set-buffer-modified-p nil)))))
    (evernote-command-with-auth create-note-func)))


(defun evernote-write-note ()
  "Write buffer to a note"
  (interactive)
  (let ((write-note-func
         (lambda ()
           (let* ((tags (evernote-completing-read-multiple "Attached Tags (comma separated form):"
                                                           (evernote-get-tag-cands (evernote-command-get-tag-list))))
                  (name (read-string "Note name:" (buffer-name)))
                  (edit-mode (completing-read "Edit Mode (type \"TEXT\" or \"XHTML\"):"
                                              '(("TEXT") ("XHTML"))
                                              nil nil "TEXT")))
             (evernote-command-create-note (current-buffer)
                                           name
                                           tags
                                           edit-mode)
             (setq evernote-note-guid (evernote-eval-command-result)
                   evernote-note-name name
                   evernote-note-tags tags
                   evernote-note-edit-mode edit-mode)
             (if (not evernote-mode)
                 (evernote-mode))
             (rename-buffer name t)
             (evernote-update-mode-line)
             (set-buffer-modified-p nil)))))
    (evernote-command-with-auth write-note-func)))


(defun evernote-edit-tags ()
  "Add or remove tags from/to the note"
  (interactive)
  (let ((edit-tags-func
         (lambda ()
           (if evernote-mode
               (let ((tags (evernote-completing-read-multiple "Change attached Tags (comma separated form):"
                                                              (evernote-get-tag-cands (evernote-command-get-tag-list))
                                                              nil nil (mapconcat #'identity evernote-note-tags ","))))
                 (setq evernote-note-tags tags)
                 (evernote-update-mode-line)
                 (set-buffer-modified-p t))))))
    (evernote-command-with-auth edit-tags-func)))


(defun evernote-change-edit-mode ()
  "Change edit mode of the note"
  (interactive)
  (let ((edit-mode (completing-read "Save as (type \"TEXT\" or \"XHTML\"):"
                                    '(("TEXT") ("XHTML"))
                                    nil t evernote-note-edit-mode)))
    (setq evernote-note-edit-mode edit-mode)
    (evernote-update-mode-line)
    (set-buffer-modified-p t)))


(defun evernote-rename-note ()
  "Rename a note"
  (interactive)
  (setq evernote-note-name (read-string "New note name:" evernote-note-name))
  (rename-buffer evernote-note-name t)
  (set-buffer-modified-p t))


(defun evernote-delete-note ()
  "Delete a note"
  (interactive)
  (if (y-or-n-p "Do you really want to remove this note? ")
      (let ((delete-note-func
             (lambda ()
               (if evernote-note-guid
                   (progn
                     (evernote-command-delete-note evernote-note-guid)
                     (kill-buffer (current-buffer)))))))
        (evernote-command-with-auth delete-note-func))))


(defun evernote-create-search ()
  "Create a saved search"
  (interactive)
	(let ((create-search-func
				 (lambda ()
					 (let (name query)
						 (setq name (read-string "Saved Search Name:"))
						 (setq query (read-string "Query:"))
						 (evernote-command-create-search name query)))))
		(evernote-command-with-auth create-search-func)))


;;
;; Helper functions.
;;

(defun evernote-command-with-auth (func &rest args)
  "Add or remove tags from/to the note"
  (let ((error-code (catch 'error (apply func args))))
    (if (or (eq error-code evernote-error-invalid-auth)
            (eq error-code evernote-error-auth-expired))
        (let ((error-code (catch 'error (evernote-command-login))))
          (if (eq error-code t)
              (apply func args)
            (message (evernote-error-message error-code))))
      (message (evernote-error-message error-code)))))


(defun evernote-get-tag-cands (tag-list)
  "Get the completion table from tag list output from command line"
  (let (acc
        (collect-tagname (lambda (node)
                           (setq acc (cons (list (cdr (assoc 'name node))) acc))
                           (let ((children (cdr (assoc 'children node))))
                             (if children
                                 (mapc collect-tagname children))))))
    (mapcar collect-tagname tag-list)
    (nreverse acc)))


(defun evernote-get-search-cands (search-list)
  "Get the completion table from search list output from command line"
	(mapcar
	 (lambda (elem) (cons (cdr (assoc 'name elem)) elem))
	 search-list))


(defun evernote-find-opened-buffer (guid)
  "Find a buffer associated with guid"
  (let ((found_buf nil))
    (save-excursion
      (mapc (lambda (buf)
              (set-buffer buf)
              (if (string= evernote-note-guid guid)
                  (setq found_buf buf)))
            (buffer-list)))
    found_buf))


(defun evernote-move-cursor-to-window (buf)
  "Move cursor to the window associated with the bufer"
  (let ((buf_window (get-buffer-window buf)))
    (if buf_window
        (select-window buf_window)
      (pop-to-buffer buf))))


(defun evernote-create-note-buffer (guid name edit-mode tags content)
  "Create new buffer for the note"
  (save-excursion
    (let ((buf (generate-new-buffer name)))
      (set-buffer buf)
      (insert content)
      (setq evernote-note-guid guid
            evernote-note-name name
            evernote-note-edit-mode edit-mode
            evernote-note-tags tags)
      (evernote-mode)
      (goto-char (point-min))
      (evernote-update-mode-line)
      (set-buffer-modified-p nil)
      (pop-to-buffer buf))))


(defun evernote-string-to-oct (string)
  "Convert the string into quoted backslashed octal edit mode."
  (concat
   "\""
   (apply 'concat (mapcar (lambda (string)
                            (format "\\%03o" string))
                          (mapcar 'identity (encode-coding-string string 'utf-8))))
   "\""))


(defun evernote-update-mode-line ()
  "Update mode line"
  (setq vc-mode
        (concat "[Tag:" (mapconcat #'identity evernote-note-tags ",") "] "
                "[Edit mode:" evernote-note-edit-mode "]"))
  (force-mode-line-update))


(defun evernote-completing-read-multiple (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  (let ((results (completing-read-multiple prompt table predicate require-match initial-input hist def inherit-input-method)))
    (delete "" results)))


(defun evernote-read-note-attr (note-list)
	"Prompts a note name and returns a note attribute"
	(let (name-set
				evernote-note-cands
				evernote-note-display-map
				evernote-note-completion-prompt)
		(mapc (lambda (attr)
						(let* ((name (cdr (assoc 'name attr)))
									 (displayed-name name)
									 (index 1))
							(while (member displayed-name name-set)
								(setq displayed-name (format "%s(%d)" name index))
								(setq index (+ index 1)))
							(setq name-set (cons displayed-name name-set))
							(setq evernote-note-cands
										(cons (cons name attr) evernote-note-cands))
							(setq evernote-note-display-map
										(cons (cons name
																(format "%s    %s"
																				(cdr (assoc 'updated attr))
																				displayed-name))
													evernote-note-display-map))))
					note-list)
		(setq evernote-note-cands (nreverse evernote-note-cands)
					evernote-note-completion-prompt (nreverse evernote-note-completion-prompt))
		(setq evernote-note-completion-prompt "Note:")
		(cdr (assoc (read-from-minibuffer evernote-note-completion-prompt
																			nil evernote-read-note-name-map)
								evernote-note-cands))))

;;		(setq evernote-note-cands
;;					(mapcar (lambda (attr)
;;										(cons (cdr (assoc 'name attr)) attr))
;;									note-list))
;;		(setq evernote-note-display-map
;;					(mapcar (lambda (attr)
;;										(cons (cdr (assoc 'name attr))
;;													(format "%s    %s"
;;																	(cdr (assoc 'updated attr))
;;																	(cdr (assoc 'name attr)))))
;;									note-list))
;;		(setq evernote-note-completion-prompt "Note:")
;;		(cdr (assoc (read-from-minibuffer evernote-note-completion-prompt
;;																			nil evernote-read-note-name-map)
;;								evernote-note-cands))))


(defun evernote-note-completion ()
	"Complete note name and display completion list"
	(interactive)
	(let (word result start)
		(setq start (+ (string-width evernote-note-completion-prompt) 1))
		(setq word (buffer-substring start (point)))
		;(evernote-tmp-message (concat "[" word "]"))
		(setq result (try-completion word evernote-note-cands))
		(cond
		 ((eq result t) (evernote-tmp-message "[Sole Completion]"))
		 ((eq result nil) (ding) (evernote-tmp-message "[No Match]"))
		 ((string= result word)
			(let (formatted-name-list)
				(setq formatted-name-list
							(mapcar (lambda (name)
												(cdr (assoc name evernote-note-display-map)))
											(all-completions word evernote-note-display-map)))
				(with-output-to-temp-buffer "*Completions*"
					(display-completion-list formatted-name-list))))
		 (t (delete-region start (point))
				(insert result)
				(if (eq t (try-completion result evernote-note-cands))
						nil
					(evernote-tmp-message "[Complete, but not unique]"))))))


(defun evernote-tmp-message (msg)
	(save-excursion
	 (goto-char (point-max))
	 (save-excursion (insert " " msg))
	 (sit-for 1)
	 (delete-region (point) (point-max))))


;;
;; Command interface.
;;

(defun evernote-command-login ()
  "Issue login command"
  (let* ((user (read-string "Evernote user name:"))
         (passwd (read-passwd "Passwd:")))
    (evernote-issue-command nil "login" user passwd)))


(defun evernote-command-get-tag-list ()
  "Issue listtags command"
  (evernote-issue-command nil "listtags")
  (evernote-eval-command-result))


(defun evernote-command-get-note-list-from-tags (tag-names)
  "Issue listnotes command from the tag name list."
  (if tag-names
      (let ((oct-tag-names
             (mapconcat #'identity (mapcar 'evernote-string-to-oct tag-names) ",")))
        (evernote-issue-command nil "listnotes" "-t" oct-tag-names))
    (evernote-issue-command nil "listnotes"))
  (evernote-eval-command-result))


(defun evernote-command-get-note-list-from-query (query)
  "Issue listnotes command from the query."
  (if query
      (let ((oct-query (evernote-string-to-oct query)))
        (evernote-issue-command nil "listnotes" "-q" oct-query))
    (evernote-issue-command nil "listnotes"))
  (evernote-eval-command-result))


(defun evernote-command-get-note-content (guid note-edit-mode)
  "Issue getnotecontent command specified by the guid and the edit mode."
  (let ((option (cond
                 ((string= note-edit-mode "XHTML") "-x")
                 ((string= note-edit-mode "TEXT") "--text"))))
    (evernote-issue-command nil "getnotecontent" guid option)
    (evernote-get-command-result)))


(defun evernote-command-create-note (inbuf name tags edit-mode)
  "Issue createnote command specified by the guid, tags and the edit-mode."
  (let (edit-mode-option)
    (cond
     ((string= edit-mode "XHTML")
      (setq edit-mode-option "-x"))
     ((string= edit-mode "TEXT")
      (setq edit-mode-option "--text")))
    (if tags
        (evernote-issue-command inbuf
                                "createnote" "-c"
                                "-t" (mapconcat #'identity (mapcar 'evernote-string-to-oct tags) ",")
                                (evernote-string-to-oct name)
                                edit-mode-option)
      (evernote-issue-command inbuf
                              "createnote" "-c"
                              (evernote-string-to-oct name)
                              edit-mode-option))))


(defun evernote-command-update-note (inbuf guid name tags edit-mode)
  "Issue updatenote command specified by the guid and the parameters for updating."
  (let (edit-mode-option)
    (cond
     ((string= edit-mode "XHTML")
      (setq edit-mode-option "-x"))
     ((string= edit-mode "TEXT")
      (setq edit-mode-option "--text")))
    (if tags
        (evernote-issue-command inbuf
                                "updatenote" "-c"
                                "-t" (mapconcat #'identity (mapcar 'evernote-string-to-oct tags) ",")
                                guid (evernote-string-to-oct name) edit-mode-option)
      (evernote-issue-command inbuf
                              "updatenote" "-c" "--delete-all-tags"
                              guid (evernote-string-to-oct name) edit-mode-option))))


(defun evernote-command-delete-note (guid)
  "Issue deletenote command specified by the guid, tags and the edit mode."
  (evernote-issue-command nil "deletenote" guid))


(defun evernote-command-get-search-list ()
  "Issue listsearch command"
  (evernote-issue-command nil "listsearch")
  (evernote-eval-command-result))


(defun evernote-command-create-search (name query)
  "Issue createsearch command"
  (evernote-issue-command nil
													"createsearch"
													(evernote-string-to-oct name)
													(evernote-string-to-oct query)))


(defun evernote-issue-command (inbuf &rest args)
  "Invoke external process to issue an evernote command."
  (let ((outbuf (get-buffer-create evernote-client-output-buffer-name))
        (infile nil)
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (if inbuf
        (save-excursion
          (set-buffer inbuf)
          (setq infile (make-temp-file "evernote"))
          (write-region (point-min) (point-max) infile)))
    (save-excursion
      (set-buffer outbuf)
      (set-buffer-file-coding-system 'utf-8)
      (erase-buffer))
    (message "Waiting for the result...")
    (let ((result (apply 'call-process "ruby" infile outbuf nil "-S" "enclient.rb" args)))
      (message "")
      (cond
       ((eq result evernote-error-ok) t)
       (t (throw 'error result))))))


(defun evernote-get-command-result ()
  "Get the result of the result of the lately issued command as a string."
  (let ((outbuf (get-buffer-create evernote-client-output-buffer-name)))
    (save-excursion
      (set-buffer outbuf)
      (set-buffer-file-coding-system 'utf-8)
      (buffer-substring (point-min) (point-max)))))


(defun evernote-eval-command-result ()
  "Get the result of the result of the lately issued command as a string and eval the string."
  (let ((outbuf (get-buffer-create evernote-client-output-buffer-name)))
    (save-excursion
      (set-buffer outbuf)
      (set-buffer-file-coding-system 'utf-8)
      (car (read-from-string
            (buffer-substring (point-min) (point-max)))))))


(defun evernote-error-message (error-code)
  "Get the error message corresponding to  the integer command result."
  (cond
   ((eq error-code evernote-error-ok)                 "OK")
   ((eq error-code evernote-error-fail)               "System error")
   ((eq error-code evernote-error-parse)              "Parse error")
   ((eq error-code evernote-error-unknown)            "Unknown error")
   ((eq error-code evernote-error-bad-data-format)    "Bad data format")
   ((eq error-code evernote-error-permission-denied)  "Permission denied")
   ((eq error-code evernote-error-internal-error)     "Internal error")
   ((eq error-code evernote-error-data-required)      "Data required")
   ((eq error-code evernote-error-limit-reached)      "Limit reached")
   ((eq error-code evernote-error-quota-reached)      "Quota reached")
   ((eq error-code evernote-error-invalid-auth)       "Invalid auth")
   ((eq error-code evernote-error-auth-expired)       "Auth expired")
   ((eq error-code evernote-error-data-conflict)      "Data conflict")
   ((eq error-code evernote-error-enml-validation)    "Enml validation. Tried to save a note of invalid format.")
   ((eq error-code evernote-error-shared-unavailable) "Shared unavailable")))


;;
;; Evernote mode
;;

(defun evernote-mode ()
  "Toggle Evernote mode, a minor mode for using evernote functions."
  (interactive)
  (or (assq 'evernote-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(evernote-mode " Evernote") minor-mode-alist)))
  (or (assq 'evernote-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
            (cons (cons 'evernote-mode evernote-mode-map) minor-mode-map-alist)))
  (set-buffer-file-coding-system 'utf-8)
  (setq evernote-mode (not evernote-mode))
  (setq vc-mode
        (concat "[Tag:" (mapconcat #'identity evernote-note-tags ",") "] "
                "[Edit Mode:" evernote-note-edit-mode "]"))
  (run-hooks 'evernote-mode-hook))


(provide 'evernote-mode)

;;(setq debug-on-error t)

