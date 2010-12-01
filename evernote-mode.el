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
;; evernote-mode home page is at: http://code.google.com/p/emacs-evernote-mode/
;; Author: Yusuke Kawakami
;; Version: 0.20
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
;;      (global-set-key "\C-ces" 'evernote-search-notes)
;;      (global-set-key "\C-ceS" 'evernote-do-saved-search)
;;      (global-set-key "\C-cew" 'evernote-write-note)
;;      (global-set-key "\C-cep" 'evernote-post-region)
;;      (global-set-key "\C-ceb" 'evernote-browser)
;;
;; There is one hooks, evernotes-mode-hook.
;; The usage of the hook is shown as follows.
;;
;; (setq evernote-mode-hook
;;   '(lambda ()
;;      (...)))

;;; Code

(require 'tree-widget)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro enh-command-with-auth (&rest body)
  "Add or remove tags from/to the note"
  `(let (error-code
         (try-func (lambda () ,@body)))
     (setq error-code
           (catch 'error
             (progn
               (funcall try-func)
               t)))
     (cond
      ((eq error-code t)
       t)
      ((or (eq error-code enh-command-error-invalid-auth)
           (eq error-code enh-command-error-auth-expired))
       (let ((error-code (catch 'error (enh-command-login))))
         (if (eq error-code t)
             (progn
               (let (error-code)
                 (catch 'error
                   (progn
                     (funcall try-func)
                     t))
                 (if (enutil-neq error-code t)
                     (enh-command-output-error error-code))))
           (enh-command-output-error error-code))))
      (t
      (enh-command-output-error error-code)))))

;;(macroexpand
;; '(enh-command-with-auth
;;   (setq a 0)
;;   (setq b 0)))


(defmacro enutil-neq (&rest exprs)
  `(not (eq ,@exprs)))


(defmacro enutil-nequal (&rest exprs)
  `(not (equal ,@exprs)))


(defmacro enutil-push (elem list)
  `(setq ,list (cons ,elem ,list)))


(defmacro enutil-pop (list)
  `(prog1
       (car ,list)
     (setq ,list (cdr ,list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for evernote-browsing-mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst evernote-browsing-mode-buffer-name "*Evernote-Browser*")

(defvar evernote-browsing-mode-map (copy-keymap global-map)
  "Keymap used in evernote browsing mode.")
(define-key evernote-browsing-mode-map "\C-m" 'widget-button-press)
(define-key evernote-browsing-mode-map "t"    'evernote-browsing-list-tags)
(define-key evernote-browsing-mode-map "S"    'evernote-browsing-list-searches)
(define-key evernote-browsing-mode-map "s"    'evernote-browsing-search-notes)
(define-key evernote-browsing-mode-map "b"    'evernote-browsing-prev-page)
(define-key evernote-browsing-mode-map "f"    'evernote-browsing-next-page)
(define-key evernote-browsing-mode-map "d"    'evernote-browsing-delete-page)
;(define-key evernote-browsing-mode-map "e"    'evernote-browsing-change-edit-mode)
;(define-key evernote-browsing-mode-map "r"    'evernote-browsing-rename-note)
;(define-key evernote-browsing-mode-map "d"    'evernote-browsing-delete-note)


(defun evernote-browsing-mode ()
  "Major mode for browsing notes."
  (interactive)
  (use-local-map evernote-browsing-mode-map)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'evernote-browsing-mode
        mode-name "Evernote-Browsing")
  (goto-char (point-min)))


(defun evernote-browser ()
  "Open an evernote browser"
  (interactive)
  (let ((buf (get-buffer evernote-browsing-mode-buffer-name)))
    (if buf
        (enutil-move-cursor-to-window buf)
      (switch-to-buffer (generate-new-buffer evernote-browsing-mode-buffer-name))
      (evernote-browsing-mode)
      (evernote-browsing-list-tags))))


(defun evernote-browsing-list-tags ()
  "List tags"
  (interactive)
  (when (eq major-mode 'evernote-browsing-mode)
    (enh-command-with-auth
     (enh-init-tag-info)
     (enh-browsing-push-page
      (enh-browsing-create-page 'tag-list "All Tags")))))


(defun evernote-browsing-list-searches ()
  "List saved searches"
  (interactive)
  (when (eq major-mode 'evernote-browsing-mode)
    (enh-command-with-auth
     (enh-init-search-info)
     (enh-browsing-push-page
      (enh-browsing-create-page 'search-list "All Saved Searches")))))


(defun evernote-browsing-search-notes ()
  "Search notes"
  (interactive)
  (when (eq major-mode 'evernote-browsing-mode)
    (let (note-attrs (query (read-string "Query:")))
      (enh-command-with-auth
       (setq note-attrs
             (enh-command-get-note-attrs-from-query query))
       (enh-set-note-attrs note-attrs)
       (enh-browsing-push-page
        (enh-browsing-create-page 'note-list
                                  (format "Query Result of: %s" query)
                                  note-attrs))))))


(defun evernote-browsing-prev-page ()
  "Move to the prev page"
  (interactive)
  (when (eq major-mode 'evernote-browsing-mode)
    (let ((prev-page (enh-browsing-get-prev-page)))
      (if prev-page
          (progn
            (setq enh-browsing-current-page prev-page)
            (enh-browsing-redraw-current-page))
        (message "[No more previous page]")))))


(defun evernote-browsing-next-page ()
  "Move to the next page"
  (interactive)
  (when (eq major-mode 'evernote-browsing-mode)
    (let ((next-page (enh-browsing-get-next-page)))
      (if next-page
          (progn
            (setq enh-browsing-current-page next-page)
            (enh-browsing-redraw-current-page))
        (message "[No more next page]")))))


(defun evernote-browsing-delete-page (&optional page)
  "Delete current page"
  (interactive)
  (when (eq major-mode 'evernote-browsing-mode)
    (unless page
      (setq page enh-browsing-current-page))
    (if (eq page enh-browsing-current-page)
        (let ((next-page (enh-browsing-get-next-page)))
          (setq enh-browsing-page-list (delq page enh-browsing-page-list))
          (if next-page
              (setq enh-browsing-current-page next-page)
            (setq enh-browsing-current-page (car enh-browsing-page-list)))
          (enh-browsing-redraw-current-page))
      (setq enh-browsing-page-list (delq page enh-browsing-page-list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for evernote-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evernote-mode nil
  "Non-nil if Evernote mode is enabled.")
(make-variable-buffer-local 'evernote-mode)

(defvar evernote-note-guid nil
  "Note guid of the buffer")
(make-variable-buffer-local 'evernote-note-guid)

(defvar evernote-mode-map (make-sparse-keymap)
  "Keymap used in evernote mode.")
(define-key evernote-mode-map "\C-x\C-s" 'evernote-save-note)
(define-key evernote-mode-map "\C-cet"   'evernote-edit-tags)
(define-key evernote-mode-map "\C-cee"   'evernote-change-edit-mode)
(define-key evernote-mode-map "\C-cer"   'evernote-rename-note)
(define-key evernote-mode-map "\C-ced"   'evernote-delete-note)


(defun evernote-mode (&optional guid)
  "Toggle Evernote mode, a minor mode for using evernote functions."
  (interactive)
  (or (assq 'evernote-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(evernote-mode " Evernote") minor-mode-alist)))
  (or (assq 'evernote-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
            (cons (cons 'evernote-mode evernote-mode-map) minor-mode-map-alist)))
  (let ((modified (buffer-modified-p)))
    (set-buffer-file-coding-system 'utf-8)
    (set-buffer-modified-p modified))
  (setq evernote-mode (not evernote-mode))
  (if evernote-mode
      (progn
        (when guid (setq evernote-note-guid guid))
        (enh-base-update-mode-line)
        (make-local-hook 'after-save-hook)
        (make-local-hook 'change-major-mode-hook)
        (add-hook 'after-save-hook
                  'evernote-mode-after-save-hook
                  nil t)
        (add-hook 'change-major-mode-hook
                  'evernote-mode-change-major-mode-hook
                  nil t)
        (run-hooks 'evernote-mode-hook))
    (progn
      (setq evernote-note-guid nil)
      (setq vc-mode nil)
      (remove-hook 'after-save-hook
                   'evernote-mode-after-save-hook)
      (remove-hook 'change-major-mode-hook
                   'evernote-mode-change-major-mode-hook))))


(defun evernote-open-note ()
  "Open a note"
  (interactive)
  (enh-command-with-auth
   (let ((note-attrs
          (enh-command-get-note-attrs-from-tag-guids
           (enh-read-tag-guids
            "Tags used for search (comma separated form. default search all tags):"))))
     (enh-update-note-attrs-external note-attrs)
     (enh-base-open-note-common (enh-base-read-note-attr note-attrs)))))


(defun evernote-search-notes ()
  "Search notes with query and open a note among them"
  (interactive)
  (let ((query (read-string "Query:")))
    (enh-command-with-auth
     (let ((note-attrs
            (enh-command-get-note-attrs-from-query
             query)))
       (enh-update-note-attrs-external note-attrs)
       (enh-base-open-note-common (enh-base-read-note-attr note-attrs))))))


(defun evernote-do-saved-search ()
  "Do a saved search and open a note"
  (interactive)
  (enh-command-with-auth
   (let ((note-attrs
          (enh-command-get-note-attrs-from-query
           (enh-read-saved-search-query))))
     (enh-update-note-attrs-external note-attrs)
     (enh-base-open-note-common (enh-base-read-note-attr note-attrs)))))


(defun evernote-create-note ()
  "Create a note"
  (interactive)
  (enh-command-with-auth
   (switch-to-buffer (enh-base-create-note-common "" t t))))


(defun evernote-write-note ()
  "Write buffer to a note"
  (interactive)
  (enh-command-with-auth
   (enh-base-create-note-common (buffer-name) nil t t)))


(defun evernote-post-region (begin end arg)
  "Post the region as a note"
  (interactive "r\np")
  (enh-command-with-auth
   (save-excursion
     (save-restriction
       (narrow-to-region begin end)
       (if (and (enutil-neq arg nil) (enutil-neq arg 1))
           (pop-to-buffer (enh-base-create-note-common (buffer-name) t t t))
         (enh-base-create-note-common (buffer-name) nil nil t))))))


(defun evernote-save-note ()
  "Save a note"
  (interactive)
  (if (and evernote-mode (buffer-modified-p))
      (enh-command-with-auth
       (enh-base-update-note-common
        (current-buffer)   ; contents
        evernote-note-guid ; guid
        nil                ; name
        t)                 ; tags
       (set-buffer-modified-p nil))
    (message "(No changes need to be saved)")))


(defun evernote-edit-tags ()
  "Add or remove tags from/to the note"
  (interactive)
  (if evernote-mode
      (enh-command-with-auth
         (let ((tag-names (enh-read-tag-names
                           "Change attached Tags (comma separated form):"
                           evernote-note-guid)))
           (enh-base-update-note-common
            nil                ; contents
            evernote-note-guid ; guid
            nil                ; name
            tag-names)))))     ; tags


(defun evernote-change-edit-mode ()
  "Change edit mode of the note"
  (interactive)
  (if evernote-mode
      (let ((edit-mode (enh-read-edit-mode evernote-note-edit-mode)))
        (enh-command-with-auth
         (enh-base-update-note-common
          (current-buffer)   ; contents
          evernote-note-guid ; guid
          nil                ; name
          t                  ; tags
          edit-mode)))))     ; edit-mode


(defun evernote-rename-note ()
  "Rename a note"
  (interactive)
  (if evernote-mode
      (let ((name (read-string "New note name:"
                               (enutil-aget 'name (enh-get-note-attr evernote-note-guid)))))
        (enh-command-with-auth
         (enh-base-update-note-common
          nil                ; contents
          evernote-note-guid ; guid
          name               ; name
          t                  ; tags
          nil))              ; edit-mode
        (rename-buffer name t)
        (enh-base-change-major-mode-from-note-name name))))


(defun evernote-delete-note ()
  "Delete a note"
  (interactive)
  (if (and evernote-mode
           (y-or-n-p "Do you really want to remove this note? "))
      (enh-command-with-auth
       (enh-command-delete-note evernote-note-guid)
       (kill-buffer (current-buffer)))))


(defun evernote-create-search ()
  "Create a saved search"
  (interactive)
  (let ((name (read-string "Saved Search Name:"))
        (query (read-string "Query:")))
  (enh-command-with-auth
   (let ((search-attr (enh-command-create-search
                       name
                       query)))
     (enh-update-search-attr search-attr)))))


(defun evernote-edit-search ()
  "Create a saved search"
  (interactive)
  (enh-command-with-auth
   (let* ((search-alist (enh-get-search-name-query-alist))
           (search-attr
            (enutil-aget
             (completing-read
              "Saved search:"
              search-alist
              nil t)
             search-alist)))
     (enh-command-update-search
      (enutil-aget 'guid search-attr)
      (read-string "New Saved search name:"
                   (enutil-aget 'name search-attr))
      (read-string "New Query:"
                   (enutil-aget 'query search-attr))))))


(defvar evernote-mode-info-for-changing-major-mode nil
  "Temporal values used when changing the major mode")


(defun evernote-mode-after-save-hook ()
  "After save hook for evernote mode. This invalid evernote-mode"
  (if evernote-mode
      (evernote-mode)))


(defun evernote-mode-change-major-mode-hook ()
  "Change major mode hook for evernote mode. This records the note info to the global variable to restore them after changing the major mode"
  (if evernote-mode
      (setq evernote-mode-info-for-changing-major-mode
            (list
             (cons 'guid  evernote-note-guid)))))


(defun evernote-mode-after-change-major-mode-hook ()
  "After change major mode hook for evernote mode. This restore the note info after changing the major mode"
  (if evernote-mode-info-for-changing-major-mode
      (progn
        (evernote-mode
         (enutil-aget 'guid evernote-mode-info-for-changing-major-mode))
        (setq evernote-mode-info-for-changing-major-mode nil))))


(add-hook 'after-change-major-mode-hook
          'evernote-mode-after-change-major-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for inputing the note name in the minibuffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evernote-read-note-map
  (copy-keymap minibuffer-local-completion-map))
(define-key evernote-read-note-map [tab] 'evernote-read-note-completion)
(define-key evernote-read-note-map "\C-i" 'evernote-read-note-completion)
(define-key evernote-read-note-map "\C-m" 'evernote-read-note-finish)
(define-key evernote-read-note-map  " "   'self-insert-command)


(defun evernote-read-note-completion ()
  "Complete note name and display completion list"
  (interactive)
  (let (word result start)
    (setq word (enutil-get-minibuffer-string))
    (setq result (try-completion word enh-base-displayed-name-attr-alist))
    (cond
     ((eq result t)
      (enutil-minibuffer-tmp-message "[Sole Completion]"))
     ((eq result nil)
      (ding)
      (enutil-minibuffer-tmp-message "[No Match]"))
     ((string= result word)
      (enh-base-display-note-completion-buf
       enh-base-displayed-name-formatted-name-alist
       word))
     (t (enutil-set-minibuffer-string result)
        (end-of-buffer)
        (if (eq t
                (try-completion result
                                enh-base-displayed-name-attr-alist))
            nil
          (enutil-minibuffer-tmp-message "[Complete, but not unique]"))))))


(defun evernote-read-note-finish ()
  "Finish input note name"
  (interactive)
  (if (assoc
       (enutil-get-minibuffer-string)
       enh-base-displayed-name-attr-alist)
      (progn
        (let ((completion-buf (get-buffer "*Evernote-Completions*")))
          (if completion-buf
              (kill-buffer completion-buf)))
        (exit-minibuffer))
    (enutil-minibuffer-tmp-message "[No Match]")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for evernote-search-mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evernote-search-mode-map (copy-keymap global-map)
  "Keymap used in evernote search mode.")
(define-key evernote-search-mode-map "\C-m" 'evernote-select-note-in-search-mode)

(defvar evernote-search-mode-formatted-name-displayed-name-alist nil
  "Alist from formatted names to names used only in evernote-search-mode buffer")
(make-variable-buffer-local 'evernote-search-mode-formatted-name-displayed-name-alist)


(defun evernote-search-mode ()
  "Major mode for selecting a note."
  (interactive)
  (use-local-map evernote-search-mode-map)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'evernote-search-mode
        mode-name "Evernote-Search")
  (goto-char (point-min)))


(defun evernote-select-note-in-search-mode ()
  "Select a note name on this buffer and input it into the mini buffer"
  (interactive)
  (if (active-minibuffer-window)
      (save-excursion
        (let (displayed-name)
          (setq displayed-name
                (enutil-aget
                 (enutil-get-current-line-string)
                 evernote-search-mode-formatted-name-displayed-name-alist))
          (if displayed-name
              (progn
                (kill-buffer (current-buffer))
                (enutil-set-minibuffer-string displayed-name)
                (exit-minibuffer)))))
    (kill-buffer (current-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for evernote-mode (enh-base-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst enh-base-enml-template
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<!DOCTYPE en-note SYSTEM \"http://xml.evernote.com/pub/enml2.dtd\">\n"
   "<en-note>\n"
   "</en-note>\n"))


(defun enh-base-open-note-common (note-attr)
  "Common procedure of opening a note"
  (let* ((note-guid (enutil-aget 'guid note-attr))
         (note-name (enutil-aget 'name note-attr))
         (note-edit-mode (enutil-aget 'edit-mode note-attr))
         (opened-buf (enh-base-find-opened-buffer note-guid)))
    (if opened-buf
        (enutil-move-cursor-to-window opened-buf)
      (let ((buf (generate-new-buffer note-name)))
        (set-buffer buf)
        (insert (enh-command-get-note-content note-guid note-edit-mode))
        (enh-base-change-major-mode-from-note-name note-name)
        (evernote-mode note-guid)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (pop-to-buffer buf)
        (enh-browsing-reflesh)))))


(defun enh-base-create-note-common (default-note-name
                                     create-new-buffer
                                     change-to-evernote-mode
                                     &optional
                                     use-current-buffer-content)
  "Common procedure of creating a note"
  (let ((tag-names (enh-read-tag-names))
        (name (read-string "Note name:" default-note-name))
        (edit-mode (enh-read-edit-mode "TEXT"))
        content
        note-attr)
    (if use-current-buffer-content
        ;; create a note from a buffer.
        (progn
          (setq note-attr
                (enh-command-create-note (current-buffer)
                                         name
                                         tag-names
                                         edit-mode))
          (setq content (buffer-substring (point-min) (point-max))))
      ;; create a note from scratch.
      (if (string= edit-mode "TEXT") ;; edit-mode = TEXT
          (setq note-attr
                (enh-command-create-note nil
                                         name
                                         tag-names
                                         edit-mode))
        (with-temp-buffer ;; edit-mode = XHTML
          (insert enh-base-enml-template)
          (setq note-attr
                (enh-command-create-note (current-buffer)
                                         name
                                         tag-names
                                         edit-mode))
          (setq content (buffer-substring (point-min) (point-max))))))
    (enh-update-note-attr note-attr)
    (let ((buf nil))
      (save-excursion
        (if create-new-buffer
            (progn
              (setq buf (generate-new-buffer name))
              (set-buffer buf)
              (when content (insert content)))
          (rename-buffer name t)) ;; BUG
        (when change-to-evernote-mode
          (set-visited-file-name nil) ; set-visited-file-name must be before (evernote-mode) because it changes the mode line.
          (enh-base-change-major-mode-from-note-name name)
          (if (not evernote-mode)
              (evernote-mode (enutil-aget 'guid note-attr))
            (enh-base-update-mode-line))
          (set-buffer-modified-p nil)))
      buf)))


(defun enh-base-update-note-common (inbuf guid &optional name tag-names edit-mode)
  "Common procedure of opening a note"
  (let ((attr (enh-get-note-attr guid)))
    (unless name
      (setq name (enutil-aget 'name attr)))
    (when inbuf
        (unless edit-mode
          (setq edit-mode (enutil-aget 'edit-mode attr))))
    (setq attr
          (enh-command-update-note inbuf guid name tag-names edit-mode))
    (enh-update-note-attr attr)))


(defun enh-base-read-note-attr (note-attrs &optional display-completion)
  "Prompts a note name and returns a note attribute"
  (let ((name-num-hash (make-hash-table :test #'equal))
        enh-base-displayed-name-attr-alist ; used in evernote-search-mode
        enh-base-displayed-name-formatted-name-alist) ; used in evernote-search-mode
    (mapc
     (lambda (attr)
       (let (name displayed-name)
         (setq name (enutil-aget 'name attr))
         (setq displayed-name
               (enh-base-get-displayed-note-name name name-num-hash))
         (setq enh-base-displayed-name-attr-alist
               (cons (cons displayed-name attr)
                     enh-base-displayed-name-attr-alist))
         (setq enh-base-displayed-name-formatted-name-alist
               (cons (cons displayed-name
                           (format "%-30s   %-15s   %s"
                                   (enutil-aget 'updated attr)
                                   (enh-tag-guids-to-comma-separated-names
                                    (enutil-aget 'tags attr)
                                    15)
                                   displayed-name))
                     enh-base-displayed-name-formatted-name-alist))))
     note-attrs)
    (setq enh-base-displayed-name-attr-alist
          (nreverse enh-base-displayed-name-attr-alist))
    (setq enh-base-displayed-name-formatted-name-alist
          (nreverse enh-base-displayed-name-formatted-name-alist))
    (if display-completion
        (enh-base-display-note-completion-buf
         enh-base-displayed-name-formatted-name-alist))
    (enutil-aget (read-from-minibuffer "Note:"
                                       nil evernote-read-note-map)
                      enh-base-displayed-name-attr-alist)))


(defun enh-base-get-displayed-note-name (name name-hash)
  "Get displayed note name from the read note name"
  (let ((num (gethash name name-num-hash))
        result)
    (if num
        (progn
          (setq num (+ num 1))
          (setq result (format "%s(%d)" name num))
          (puthash name num name-num-hash))
      (setq result (substring name 0))
      (puthash name 1 name-num-hash))
    result))


(defun enh-base-display-note-completion-buf (displayed-name-formatted-name-alist &optional word)
  (let (formatted-name-displayed-name-alist completion-buf)
    (setq formatted-name-displayed-name-alist
          (mapcar (lambda (displayed-name)
                    (cons
                     (enutil-aget
                      displayed-name
                      enh-base-displayed-name-formatted-name-alist)
                     displayed-name))
                  (all-completions
                   (if word
                       word
                     "")
                   enh-base-displayed-name-formatted-name-alist)))
    (save-excursion
      (setq completion-buf (get-buffer-create "*Evernote-Completions*"))
      (set-buffer completion-buf)
      (enh-base-display-note-completion-list
       formatted-name-displayed-name-alist)
      (setq evernote-search-mode-formatted-name-displayed-name-alist
            formatted-name-displayed-name-alist)
      (evernote-search-mode))
    (display-buffer completion-buf)))


(defun enh-base-display-note-completion-list (formatted-name-displayed-name-alist)
  "Display formatted note names on this buffer"
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (format "total %d\n%-30s   %-15s   %s\n\n"
                  (length formatted-name-displayed-name-alist)
                  "Last Modified"
                  "Tags"
                  "Title"))
  (mapc (lambda (elem)
          (insert (car elem) "\n"))
        formatted-name-displayed-name-alist)
  (setq buffer-read-only t))


(defun enh-base-change-major-mode-from-note-name (note-name)
  (catch 'mode
    (mapc
     (lambda (pattern-mode-pair)
       (if (consp pattern-mode-pair)
           (let ((pattern (car pattern-mode-pair))
                 (mode (cdr pattern-mode-pair)))
             (if (consp mode)
                 (setq mode (car mode)))
             (if (and (stringp pattern)
                      (fboundp mode)
                      (string-match pattern note-name))
                 (progn
                   (funcall mode)
                   (throw 'mode mode))))))
     auto-mode-alist)
    (if (fboundp default-major-mode)
        (progn
          (funcall default-major-mode)
          (throw 'mode default-major-mode)))))


;;(defun enh-base-all-reflesh ()
;;  (mapc
;;   (lambda (buf)
;;     (save-excursion
;;       (set-buffer buf)
;;       (if evernote-mode
;;           (enh-base-reflesh))))
;;   (buffer-list)))


;;(defun enh-base-reflesh ()
;;  (if evernote-mode
;;      (let (note-attr name tag-names edit-mode)
;;        (setq note-attr (enh-get-note-attr evernote-note-guid))
;;        (setq name (enutil-aget 'name note-attr))
;;        (setq tag-names (evernote-tag-guids-to-comma-separated-names
;;                         (enutil-aget 'tag-list note-attr)))
;;        (setq edit-mode (enutil-aget 'edit-mode note-attr))
;;        (rename-buffer name t)
;;        (enh-base-update-mode-line name tag-names edit-mode))))


(defun enh-base-find-opened-buffer (guid)
  "Find a buffer associated with guid"
  (let ((found_buf nil))
    (save-excursion
      (mapc (lambda (buf)
              (set-buffer buf)
              (if (string= evernote-note-guid guid)
                  (setq found_buf buf)))
            (buffer-list)))
    found_buf))


(defun enh-base-update-mode-line ()
  "Update mode line"
  (let ((note-attr (enh-get-note-attr evernote-note-guid)))
    (setq vc-mode
          (concat "[Tag:"
                  (enh-tag-guids-to-comma-separated-names
                   (enutil-aget 'tags note-attr))
                  "] "
                  "[Edit mode:"
                  (enutil-aget 'edit-mode note-attr)
                  "]"))
    (force-mode-line-update)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for evernote-browsing-mode (enh-browsing-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar enh-browsing-page-list nil
  "Page data list of evernote browsing mode.")
(make-variable-buffer-local 'enh-browsing-page-list)

(defvar enh-browsing-current-page nil
  "Current page data of evernote browsing mode.")
(make-variable-buffer-local 'enh-browsing-current-page)


(defun enh-browsing-open-tag (widget &rest ignored)
  "Open a tag in browsing mode"
  (let* ((guid (widget-value widget))
         (note-attrs
          (enh-command-get-note-attrs-from-tag-guids
           (if guid
               (list guid)
             nil))))
    (enh-set-note-attrs note-attrs)
    (enh-browsing-push-page
     (enh-browsing-create-page 'note-list
                               (format "Notes with tag %s"
                                       (enutil-aget 'name
                                                    (enh-get-tag-attr guid)))
                               note-attrs))))


(defun enh-browsing-open-search (widget &rest ignored)
  "Open a saved search in browsing mode"
  (let* ((guid (widget-value widget))
         (note-attrs
          (enh-command-get-note-attrs-from-query
           (enutil-aget 'query
                        (enh-get-search-attr guid)))))
    (enh-set-note-attrs note-attrs)
    (enh-browsing-push-page
     (enh-browsing-create-page 'note-list
                               (format "Query Result of Saved Search: %s"
                                       (enutil-aget 'name
                                                    (enh-get-search-attr guid)))
                               note-attrs))))


(defun enh-browsing-open-note (widget &rest ignored)
  "Open a note in browsing mode"
  (let ((guid (widget-value widget)))
    (enh-base-open-note-common
     (enh-get-note-attr guid))))


(defun enh-browsing-create-page (type description &optional attr-list)
  "Create a page structure of the attr-list"
  (list
   (cons 'type type)
   (cons 'description description)
   (cons 'guid-list
         (mapcar
          (lambda (attr)
            (enutil-aget 'guid attr))
          attr-list))))


(defun enh-browsing-reflesh ()
  "Refresh browsing buffer"
  (let ((buf (get-buffer evernote-browsing-mode-buffer-name)))
    (when buf
      (save-excursion
        (set-buffer buf)
        (enh-browsing-redraw-current-page)))))


(defun enh-browsing-redraw-current-page ()
  "Draw browsing buffer"
  (let (current-page type)
    (save-excursion
      (if enh-browsing-current-page
          (progn
            (setq type (enutil-aget 'type enh-browsing-current-page))
            (cond
             ((eq type 'tag-list)
              (enh-browsing-draw-tag-list-page))
             ((eq type 'search-list)
              (enh-browsing-draw-search-list-page))
             ((eq type 'note-list)
              (enh-browsing-draw-note-list-page))))
        (enutil-erase-buffer-forcibly)))))


(defun enh-browsing-draw-tag-list-page ()
  "Create a page structure of the attr-list"
  (erase-buffer)
  (widget-insert (format "Tag List\n\ntotal %d\n" (hash-table-count enh-tag-info)))
  (let ((guid-children-hash (make-hash-table :test #'equal)))
    (maphash
     (lambda (guid attr)
       (let* ((parent (enutil-aget 'parent attr))
              (children (gethash parent guid-children-hash)))
       (if children
           (puthash parent (cons guid children) guid-children-hash)
         (puthash parent (list guid) guid-children-hash))))
     enh-tag-info)
    (message "test")
    (apply 'widget-create
           (enh-browsing-get-tag-tree nil)))
  (widget-setup))


(defun enh-browsing-get-tag-tree (guid) ; root (eq guid nil)
  (let* ((children (gethash guid guid-children-hash))
         (attr (enh-get-tag-attr guid))
         (name (if attr (enutil-aget 'name attr) "All tags")))
    (if children
        `(tree-widget :node (push-button :tag ,name
                                         :format "%[%t%]\n"
                                         :notify enh-browsing-open-tag
                                         ,guid)
                      :args ,(mapcar
                              'enh-browsing-get-tag-tree
                              (nreverse children))
                      :open ,(if attr nil t))
      `(push-button :tag ,name
                    :format "%[%t%]\n"
                    :notify enh-browsing-open-tag
                    ,guid))))


(defun enh-browsing-test-open-tag (widget &rest ignored)
  (message (widget-value widget)))


(defun enh-browsing-draw-search-list-page ()
  "Insert saved search list into the browsing buffer"
  (erase-buffer)
  (widget-insert
   (format "%s\n\ntotal %d\n%-30s   %s\n\n"
           (enutil-aget 'description enh-browsing-current-page)
           (hash-table-count enh-search-info)
           "Name"
           "Query"))
  (maphash
   (lambda (guid attr)
     (let ((attr (enh-get-search-attr guid)))
       (widget-create 'push-button
                      :tag (format "%-30s   %s"
                                   (enutil-aget 'name attr)
                                   (enutil-aget 'query attr))
                      :format "%[%t%]\n"
                      :notify enh-browsing-open-search
                      guid)))
   enh-search-info)
  (widget-setup))


(defun enh-browsing-draw-note-list-page ()
  "Insert note list into the browsing buffer"
  (erase-buffer)
  (let ((guid-list (enutil-aget 'guid-list enh-browsing-current-page))
        (note-attrs (make-hash-table :test #'equal)))
    (mapc
     (lambda (guid)
       (puthash guid (gethash guid enh-note-info) note-attrs))
     guid-list)
    (widget-insert
     (format "%s\n\ntotal %d\n%-30s   %-15s   %s\n\n"
             (enutil-aget 'description enh-browsing-current-page)
             (hash-table-count note-attrs)
             "Last Modified"
             "Tags"
             "Name"))
    (maphash
     (lambda (guid attr)
       (widget-create 'push-button
                      :tag (format "%-30s   %-15s   %s"
                                   (enutil-aget 'updated attr)
                                   (enh-tag-guids-to-comma-separated-names
                                    (enutil-aget 'tags attr)
                                    15)
                                   (enutil-aget 'name attr))
                      :format "%[%t%]\n"
                      :notify enh-browsing-open-note
                      guid))
     note-attrs))
  (widget-setup))


(defun enh-browsing-push-page (page)
  "Push a new page to the browsing mode"
  (let ((type (enutil-aget 'type page)))
    (if (or (eq type 'tag-list) (eq type 'search-list))
        (let (same-type-page)
          (setq same-type-page
                (enh-browsing-get-page-of-type type))
          (if same-type-page
              (evernote-browsing-delete-page same-type-page)))))
  (enutil-push page enh-browsing-page-list)
  (setq enh-browsing-current-page page)
  (enh-browsing-redraw-current-page))


(defun enh-browsing-get-page-of-type(type)
  "Get a page of the type in the browsing mode"
  (catch 'result
    (mapc
     (lambda (page)
       (if (eq (enutil-aget 'type page) type)
           (throw 'result page)))
     enh-browsing-page-list)
    nil))


(defun enh-browsing-get-current-page-type ()
  (enutil-aget
   'type
   enh-browsing-current-page))


(defun enh-browsing-get-current-page-guids ()
  (enutil-aget
   'guids
   enh-browsing-current-page))


(defun enh-browsing-get-next-page ()
  (if (enutil-neq enh-browsing-current-page (car enh-browsing-page-list))
      (catch 'next-page
        (let (next-page)
          (mapc
           (lambda (page)
             (when (eq page enh-browsing-current-page)
               (throw 'next-page next-page))
             (setq next-page page))
           enh-browsing-page-list)))))


(defun enh-browsing-get-prev-page ()
  (cadr
   (memq enh-browsing-current-page
         enh-browsing-page-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for executing the external command (enh-command-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst enh-command-output-buffer-name "*Evernote-Client-Output*")

;;
;; Const variables.
;;

(defconst enh-command-error-ok                  0)
(defconst enh-command-error-fail                100)
(defconst enh-command-error-parse               101)
(defconst enh-command-error-unknown             1)
(defconst enh-command-error-bad-data-format     2)
(defconst enh-command-error-permission-denied   3)
(defconst enh-command-error-internal-error      4)
(defconst enh-command-error-data-required       5)
(defconst enh-command-error-limit-reached       6)
(defconst enh-command-error-quota-reached       7)
(defconst enh-command-error-invalid-auth        8)
(defconst enh-command-error-auth-expired        9)
(defconst enh-command-error-data-conflict      10)
(defconst enh-command-error-enml-validation    11)
(defconst enh-command-error-shared-unavailable 12)


(defun enh-command-login ()
  "Issue login command"
  (let* ((user (read-string "Evernote user name:"))
         (passwd (read-passwd "Passwd:")))
    (enh-command-issue nil "login" user passwd)))


(defun enh-command-get-tag-attrs ()
  "Issue listtags command"
  (enh-command-issue nil "listtags")
  (enh-command-eval-result))


(defun enh-command-get-note-attrs-from-tag-guids (tag-guids)
  "Issue listnotes command from the tag name list."
  (if tag-guids
      (enh-command-issue nil
                         "listnotes" "-t"
                         (mapconcat #'identity tag-guids ","))
    (enh-command-issue nil "listnotes"))
  (enh-command-eval-result))


(defun enh-command-get-note-attrs-from-query (query)
  "Issue listnotes command from the query."
  (if query
      (let ((oct-query (enutil-string-to-oct query)))
        (enh-command-issue nil "listnotes" "-q" oct-query))
    (enh-command-issue nil "listnotes"))
  (enh-command-eval-result))


(defun enh-command-get-note-content (guid note-edit-mode)
  "Issue getnotecontent command specified by the guid and the edit mode."
  (let ((option (cond
                 ((string= note-edit-mode "XHTML") "-x")
                 ((string= note-edit-mode "TEXT") "--text"))))
    (enh-command-issue nil "getnotecontent" guid option)
    (enh-command-get-result)))


(defun enh-command-create-note (inbuf name tag-names edit-mode)
  "Issue createnote command specified by the guid, tags and the edit-mode."
  (let (edit-mode-option)
    (cond
     ((string= edit-mode "XHTML")
      (setq edit-mode-option "-x"))
     ((string= edit-mode "TEXT")
      (setq edit-mode-option "--text")))
    (if tag-names
        (enh-command-issue inbuf
                           "createnote" "-c"
                           "-t" (enh-tag-names-to-comma-separated-oct-names tag-names)
                           (enutil-string-to-oct name)
                           edit-mode-option)
      (enh-command-issue inbuf
                         "createnote" "-c"
                         (enutil-string-to-oct name)
                         edit-mode-option)))
  (enh-command-eval-result))


(defun enh-command-update-note (inbuf guid name tag-names edit-mode)
  "Issue updatenote command specified by the guid and the parameters for updating."
  (let (command)
    (enutil-push "updatenote" command)
    (enutil-push guid command)
    (enutil-push (enutil-string-to-oct name) command)
    (when inbuf
      (enutil-push "-c" command))
    (cond
     ((listp tag-names)
      (enutil-push "-t" command)
      (enutil-push (enh-tag-names-to-comma-separated-oct-names-names) command))
     ((eq tag-names nil)
      (enutil-push "--delete-all-tags" command)))
    (cond
     ((string= edit-mode "XHTML") 
      (enutil-push "-x" command))
     ((string= edit-mode "TEXT")
      (enutil-push "--text" command)))
    (setq command (nreverse command))
    (apply 'enh-command-issue inbuf command))
  (enh-command-eval-result))


(defun enh-command-delete-note (guid)
  "Issue deletenote command specified by the guid, tags and the edit mode."
  (enh-command-issue nil "deletenote" guid))


(defun enh-command-get-search-attrs ()
  "Issue listsearch command"
  (enh-command-issue nil "listsearch")
  (enh-command-eval-result))


(defun enh-command-create-search (name query)
  "Issue createsearch command"
  (enh-command-issue nil
                     "createsearch"
                     (enutil-string-to-oct name)
                     (enutil-string-to-oct query)))


(defun enh-command-update-search (guid name query)
  "Issue updatesearch command"
  (enh-command-issue nil
                     "updatesearch"
                     guid
                     (enutil-string-to-oct name)
                     (enutil-string-to-oct query)))


(defun enh-command-issue (inbuf &rest args)
  "Invoke external process to issue an evernote command."
  (let ((outbuf (get-buffer-create enh-command-output-buffer-name))
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
       ((eq result enh-command-error-ok) t)
       (t (throw 'error result))))))


(defun enh-command-get-result ()
  "Get the result of the result of the lately issued command as a string."
  (let ((outbuf (get-buffer-create enh-command-output-buffer-name)))
    (save-excursion
      (set-buffer outbuf)
      (set-buffer-file-coding-system 'utf-8)
      (buffer-substring (point-min) (point-max)))))


(defun enh-command-eval-result ()
  "Get the result of the result of the lately issued command as a string and eval the string."
  (let ((outbuf (get-buffer-create enh-command-output-buffer-name)))
    (save-excursion
      (set-buffer outbuf)
      (set-buffer-file-coding-system 'utf-8)
      (car (read-from-string
            (buffer-substring (point-min) (point-max)))))))


(defun enh-command-output-error (error-code)
  (message "%s (%s)"
           (enh-command-error-message error-code)
           (enutil-get-first-line
            (enh-command-get-result))))


(defun enh-command-error-message (error-code)
  "Get the error message corresponding to  the integer command result."
  (cond
   ((eq error-code enh-command-error-ok)                 "OK")
   ((eq error-code enh-command-error-fail)               "System error")
   ((eq error-code enh-command-error-parse)              "Parse error")
   ((eq error-code enh-command-error-unknown)            "Unknown error")
   ((eq error-code enh-command-error-bad-data-format)    "Bad data format")
   ((eq error-code enh-command-error-permission-denied)  "Permission denied")
   ((eq error-code enh-command-error-internal-error)     "Internal error")
   ((eq error-code enh-command-error-data-required)      "Data required")
   ((eq error-code enh-command-error-limit-reached)      "Limit reached")
   ((eq error-code enh-command-error-quota-reached)      "Quota reached")
   ((eq error-code enh-command-error-invalid-auth)       "Invalid auth")
   ((eq error-code enh-command-error-auth-expired)       "Auth expired")
   ((eq error-code enh-command-error-data-conflict)      "Data conflict. The data already exists.")
   ((eq error-code enh-command-error-enml-validation)    "Enml validation. Tried to save a note of invalid format.")
   ((eq error-code enh-command-error-shared-unavailable) "Shared unavailable")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for all modes in this file (enh-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar enh-tag-info (make-hash-table :test #'equal)
  "Tag info associated with the guid")

(defvar enh-search-info (make-hash-table :test #'equal)
  "Saved search info associated with the guid")

(defvar enh-note-info (make-hash-table :test #'equal)
  "Note info associated with the guid")


(defun enh-get-tag-attr (guid)
  "Get the tag attr from the guid"
  (gethash guid enh-tag-info))


(defun enh-set-tag-attr (attr)
  "Set the tag attr of the guid"
  (puthash (enutil-aget 'guid attr) attr enh-tag-info))


(defun enh-get-search-attr (guid)
  "Get the search attr from the guid"
  (gethash guid enh-search-info))


(defun enh-set-search-attr (attr)
  "Set the tag attr of the guid"
  (puthash (enutil-aget 'guid attr) attr enh-search-info))


(defun enh-get-note-attr (guid)
  "Get the note attr from the guid"
  (gethash guid enh-note-info))


(defun enh-set-note-attr (attr)
  "Set the tag attr of the guid"
  (puthash (enutil-aget 'guid attr) attr enh-note-info))


(defun enh-set-tag-attrs (tag-attrs)
  "Set the tag attrs in the tag-list"
  (mapc
   (lambda (attr)
     (enh-set-tag-attr attr))
   tag-attrs))


(defun enh-set-search-attrs (search-attrs)
  "Set the search attrs in the search-list"
  (mapc
   (lambda (attr)
     (enh-set-search-attr attr))
   search-attrs))


(defun enh-set-note-attrs (note-attrs)
  "Set the note attrs in the search-list"
  (mapc
   (lambda (attr)
     (enh-set-note-attr attr))
   note-attrs))


(defun enh-read-tag-guids (&optional prompt)
  (enh-init-tag-info)
  (enh-tag-names-to-guids
   (enutil-completing-read-multiple
    (if prompt
        prompt
      "Tags (comma separated form):")
    (enh-get-tag-name-alist)
    nil
    t)))


(defun enh-read-tag-names (&optional prompt note-guid)
  (enh-init-tag-info)
  (enutil-completing-read-multiple
   (if prompt
       prompt
     "Tags (comma separated form):")
   (enh-get-tag-name-alist)
   nil
   nil
   (if note-guid
       (enh-tag-guids-to-comma-separated-names
        (enutil-aget 'tags
                     (enh-get-tag-attr note-guid)))
     nil)))


(defun enh-reset-local-cache ()
  (interactive)
  (setq enh-tag-info (make-hash-table :test #'equal)
        enh-search-info (make-hash-table :test #'equal)
        enh-note-info (make-hash-table :test #'equal)))


(defun enh-read-saved-search-query (&optional prompt)
  (enh-init-search-info)
  (let ((search-name-query-alist (enh-get-search-name-query-alist)))
    (enutil-aget 'query
                 (enutil-aget (completing-read
                               (if prompt
                                   prompt
                                 "Saved search:")
                               search-name-query-alist
                               nil t)
                              search-name-query-alist))))


(defun enh-get-tag-name-alist ()
  "Get the tag alist for completion from command output"
  (enh-init-tag-info)
  (let (result)
    (maphash
     (lambda (guid attr)
       (setq result
             (cons
              (list (enutil-aget 'name attr))
              result)))
     enh-tag-info)
    result))


(defun enh-get-search-name-query-alist ()
  "Get the alist for completion from command output"
  (enh-init-search-info)
  (let (result)
    (maphash
     (lambda (guid attr)
       (setq result
             (cons
              (cons (enutil-aget 'name attr)
                    (enutil-aget 'query attr))
              result)))
     enh-search-info)
    result))


(defun enh-init-tag-info ()
  (if (eq (hash-table-count enh-tag-info) 0)
      (enh-set-tag-attrs
       (enh-command-get-tag-attrs))))


(defun enh-init-search-info ()
  (if (eq (hash-table-count enh-search-info) 0)
      (enh-set-search-attrs
       (enh-command-get-search-attrs))))


(defun enh-update-note-attrs-external (note-attrs)
  (enh-set-note-attrs note-attrs))


(defun enh-update-note-attr (note-attr)
  (enh-set-note-attr note-attr)
  (let* ((tag-guids (enutil-aget 'guid note-attr)))
    (if (catch 'result
            (mapc
             (lambda (guid)
               (unless (gethash guid enh-tag-info)
                 (throw 'result t)))
             tag-guids))
        (progn
          (enh-set-tag-attrs
           (enh-command-get-tag-attrs))
          (enh-browsing-reflesh)))))


(defun enh-update-tag-attrs-external (tag-attrs)
  (enh-set-tag-attrs tag-attrs)
  (enh-browsing-reflesh))


(defun enh-update-search-attrs-external (search-attrs)
  (enh-set-search-attrs search-attrs)
  (enh-browsing-reflesh))


(defun enh-update-search-attr (search-attr)
  (enh-set-search-attr search-attr)
  (enh-browsing-reflesh))


(defun enh-tag-guids-to-comma-separated-names (tag-guids &optional maxlen)
  (let (line)
    (setq line
          (mapconcat
           (lambda (guid)
             (enutil-aget 'name (enh-get-tag-attr guid)))
           tag-guids
           ","))
    (if maxlen
        (truncate-string-to-width line maxlen)
      line)))


(defun enh-tag-names-to-comma-separated-oct-names (tag-names)
  (mapconcat #'identity (mapcar 'enutil-string-to-oct tag-names) ","))


;;(defun enh-tag-guids-to-names (tag-guids)
;;  (mapcar
;;   (lambda (tag-guid)
;;     (enutil-aget 'name (enh-get-tag-attr tag-guid)))
;;   tag-guids))


(defun enh-tag-names-to-guids (tag-names)
  (mapcar
   (lambda (name)
     (catch 'guid
       (maphash
        (lambda (guid attr)
          (let ((tag-name (enutil-aget 'name attr)))
            (if (equal tag-name name)
                (throw 'guid guid))))
        enh-tag-info)))
   tag-names))


(defun enh-read-edit-mode (default)
  (completing-read "Edit mode (type \"TEXT\" or \"XHTML\"):"
                   '(("TEXT") ("XHTML"))
                   nil t default))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General util functions (enutil-xxx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun enutil-completing-read-multiple
  (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  "Read multiple strings with completion. and return nil if no input is given"
  (let (results)
    (setq results
          (completing-read-multiple prompt
                                    table
                                    predicate
                                    require-match
                                    initial-input
                                    hist
                                    def
                                    inherit-input-method))
    (delete "" results)))


(defun enutil-aget (key alist)
  (let ((result-cons (assoc key alist)))
    (when result-cons
      (cdr result-cons))))


(defun enutil-get-current-line-string ()
  (save-excursion
    (buffer-substring
     (progn
       (beginning-of-line)
       (point))
     (progn
       (end-of-line)
       (point)))))


(defun enutil-erase-buffer-forcibly ()
  (let ((buffer-read-only nil))
    (erase-buffer)))


(defun enutil-get-first-line (str)
  "Get first line of the string"
  (let ((begin (string-match "^.*$" str)))
    (substring str begin (match-end 0))))


(defun enutil-get-minibuffer-string ()
  (save-excursion
    (enutil-set-buffer-to-minibuffer)
    (buffer-substring
     (progn
       (goto-char (+ 1 (minibuffer-prompt-width)))
       (point))
     (progn
       (end-of-line)
       (point)))))


(defun enutil-set-minibuffer-string (str)
  (save-excursion
    (enutil-set-buffer-to-minibuffer)
    (delete-region
     (progn
       (goto-char (+ 1 (minibuffer-prompt-width)))
       (point))
     (progn
       (end-of-line)
       (point)))
    (insert str)))


(defun enutil-set-buffer-to-minibuffer ()
  (set-buffer (window-buffer (active-minibuffer-window))))


(defun enutil-minibuffer-tmp-message (msg)
  (save-excursion
    (goto-char (point-max))
    (save-excursion (insert " " msg))
    (sit-for 1)
    (delete-region (point) (point-max))))


(defun enutil-move-cursor-to-window (buf)
  "Move cursor to the window associated with the bufer"
  (let ((buf-window (get-buffer-window buf)))
    (if buf-window
        (select-window buf-window)
      (pop-to-buffer buf))))


(defun enutil-string-to-oct (string)
  "Convert the string into quoted backslashed octal edit mode."
  (concat
   "\""
   (apply 'concat (mapcar (lambda (string)
                            (format "\\%03o" string))
                          (mapcar 'identity (encode-coding-string string 'utf-8))))
   "\""))


(provide 'evernote-mode)

;;(setq debug-on-error t)

