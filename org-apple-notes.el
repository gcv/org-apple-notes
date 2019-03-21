;;; org-apple-notes.el --- Org to Apple Notes synchronizer  -*- lexical-binding: t; -*-

;;; Customization

(defgroup org-apple-notes nil
  "Customization options for org-apple-notes."
  :group 'external)

(defcustom org-apple-notes-completing-read-fn #'completing-read
  "ido users should set this to ido-completing-read.
Helm users should set this to helm-comp-read.
Ivy users should set this to ivy-completing-read."
  :group 'org-apple-notes
  :type 'function)


;;; Support

(setq org-apple-notes--applescript-tmpl-base "
on join(itemlist, delim)
  set olddelims to AppleScript's text item delimiters
  set str to \"\"
  set AppleScript's text item delimiters to delim
  set str to itemlist as string
  set AppleScript's text item delimiters to olddelims
  return str
end join

on replace(str, src, target)
  set olddelims to AppleScript's text item delimiters
  set AppleScript's text item delimiters to the src
  set the itemlist to every text item of str
  set AppleScript's text item delimiters to the target
  set newstr to the itemlist as string
  set AppleScript's text item delimiters to olddelims
  return newstr
end replace

on escape(str)
  return my replace(str, \"\\\"\", \"\\\\\\\"\")
end escape

on qe(str)
  return \"\\\"\" & my escape(str) & \"\\\"\"
end qe
")

(setq org-apple-notes--applescript-tmpl-list-notes "
tell application \"Notes\"
  set accnames to the name of every account
  set realaccounts to {}
  repeat with h from 1 to (count of accnames)
    set accname to (item h of accnames)
    tell account accname
      set foldernames to the name of every folder
      set realfolders to {}
      repeat with i from 1 to (count of foldernames)
        set foldername to (item i of foldernames)
        tell folder foldername
          set notenames to the name of every note
          set realnotes to {}
          repeat with j from 1 to (count of notenames)
            set notename to (item j of notenames)
            set end of realnotes to (my qe(notename))
          end repeat
          set outnotes to (\"[\" & (my join(realnotes, \",\")) & \"]\")
        end tell
        set end of realfolders to (my qe(foldername) & \":\" & outnotes)
      end repeat
      set outfolders to (\"{\" & (my join(realfolders, \",\")) & \"}\")
    end tell
    set end of realaccounts to (my qe(accname) & \":\" & outfolders)
  end repeat
  set outputaccounts to (\"{\" & (my join(realaccounts, \",\")) & \"}\")
end tell
outputaccounts
")

(setq org-apple-notes--applescript-tmpl-note-exists "
set argacct to \"%s\"
set argfolder to \"%s\"
set argnote to \"%s\"
tell application \"Notes\"
  tell account argacct
    tell folder argfolder
      if (note named argnote exists) then
        \"true\"
      else
        \"false\"
      end if
    end tell
  end tell
end tell
")

(setq org-apple-notes--applescript-tmpl-read-note "
set argacct to \"%s\"
set argfolder to \"%s\"
set argnote to \"%s\"
tell application \"Notes\"
  tell account argacct
    tell folder argfolder
      body of note argnote
    end tell
  end tell
end tell
")

(setq org-apple-notes--applescript-tmpl-write-note "
set argacct to \"%s\"
set argfolder to \"%s\"
set argnote to \"%s\"
set argtext to \"%s\"
tell application \"Notes\"
  tell account argacct
    tell folder argfolder
      if not (note named argnote exists) then
        make new note with properties {name: argnote}
      end if
      set body of note argnote to argtext
    end tell
  end tell
end tell
")

(defun org-apple-notes--applescript (str)
  (declare (indent 0))
  (let ((scpt (format "%s\n%s" org-apple-notes--applescript-tmpl-base str)))
    (do-applescript scpt)))

(defun org-apple-notes--list-apple-notes ()
  "Returns all Notes documents."
  (let* ((notes-raw (org-apple-notes--applescript
                      org-apple-notes--applescript-tmpl-list-notes))
         (notes (json-read-from-string notes-raw)))
    notes))

(defun org-apple-notes--select-apple-note-from-keywords ()
  (let ((parsed-buffer (org-element-parse-buffer))
        (note-vars (make-hash-table :test 'equal)))
    (org-element-map parsed-buffer
        'keyword (lambda (elt) (puthash (downcase (org-element-property :key elt)) (org-element-property :value elt) note-vars)))
    (list (gethash "apple-notes-account" note-vars)
          (gethash "apple-notes-folder" note-vars)
          (or (gethash "apple-notes-title" note-vars)
              (gethash "title" note-vars)))))

(defun org-apple-notes--select-apple-note-interactively ()
  (let ((notes (org-apple-notes--list-apple-notes))
        (notes-for-completing-read (make-hash-table :test 'equal))
        (display-names (list)))
    ;; construct a map where the keys are display strings for note selection and
    ;; the values are lists consisting of the uniquely combination of account,
    ;; folder, and note title
    (mapc (lambda (account)
            (let ((account-name (car account))
                  (account-folders (cdr account)))
              (mapc (lambda (folder)
                      (let ((folder-name (car folder))
                            (note-names (cdr folder)))
                        (mapc (lambda (note-name)
                                (let* ((args (list account-name folder-name note-name))
                                       (display-name (apply #'format "%s / %s / %s" args)))
                                  (push display-name display-names)
                                  (puthash display-name args notes-for-completing-read)))
                              note-names)))
                    account-folders)))
          notes)
    (let ((selected-note (funcall org-apple-notes-completing-read-fn
                                  "Select Apple note: "
                                  (sort display-names #'string-lessp))))
      ;; TODO: Allow writing a new note. Prompt individually for account,
      ;; folder, and note name.
      (gethash selected-note notes-for-completing-read))))

(defun org-apple-notes--select-apple-note ()
  (let ((from-keywords (org-apple-notes--select-apple-note-from-keywords)))
    (seq-let [account folder title] from-keywords
      (if (and account folder title)
          from-keywords
        (org-apple-notes--select-apple-note-interactively)))))

(defun org-apple-notes--apple-note-exists-p (account folder note)
  "Checks if a Notes document exists."
  (let ((res (org-apple-notes--applescript
               (format org-apple-notes--applescript-tmpl-note-exists
                       account folder note))))
    (string-equal "true" res)))

(defun org-apple-notes--read-apple-note (account folder note)
  "Read a Notes document."
  (org-apple-notes--applescript
    (format org-apple-notes--applescript-tmpl-read-note
            account folder note)))

(defun org-apple-notes--write-apple-note (account folder note new-contents)
  "Destructively write a Notes document."
  (org-apple-notes--applescript
    (format org-apple-notes--applescript-tmpl-write-note
            account folder note
            ;; escape backslashes, then escape double quotes
            (replace-regexp-in-string "\"" "\\\\\""
                                      (replace-regexp-in-string
                                       "\\\\" "\\\\\\\\" new-contents)))))

(defun org-apple-notes--replace-regex-in-buffer (rx replacement)
  "Delete the given regex everywhere in the current buffer."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward rx nil t)
        (replace-match replacement)))))


;;; Commands

;;;###autoload
(defun org-apple-notes-push ()
  "Save the current buffer's contents in a Notes document.

Determine the Notes document to write to as follows:

- if the APPLE-NOTES-ACCOUNT, APPLE-NOTES-FOLDER, and
  APPLE-NOTES-TITLE or TITLE keywords are set in the Org file,
  use them (i.e., #+APPLE-NOTES-FOLDER: Some Folder)
- otherwise, prompt using org-apple-notes-completing-read-fn

If the Notes document exists, prompt to overwrite it. With a
prefix argument, do not prompt and force overwrite.
"
  (interactive)
  (if (not (eq 'org-mode major-mode))
      (message "not in Org mode")
    (seq-let [account folder note] (org-apple-notes--select-apple-note)
      (when (or current-prefix-arg
                (not (org-apple-notes--apple-note-exists-p account folder note))
                (yes-or-no-p (format "%s / %s / %s exists! Overwrite? " account folder note)))
        (let ((title (file-name-base (buffer-file-name)))
              (tmp-buffer-name (format "*%04x%04x*" (random (expt 16 4)) (random (expt 16 4))))
              (org-export-with-section-numbers nil)
              (org-export-with-author nil)
              (org-export-with-date nil)
              (org-export-with-statistics-cookies nil)
              (org-export-with-toc nil)
              (org-export-with-title nil)
              (org-html-preamble nil)
              (org-html-postamble nil)
              (org-html-head-include-scripts nil)
              (org-html-head-include-default-style nil)
              (org-html-xml-declaration '(("html" . "")))
              (org-html-doctype "")
              (org-html-checkbox-type 'ascii)
              (org-html-toplevel-hlevel 2))
          (unwind-protect
               (with-current-buffer (org-export-to-buffer 'html tmp-buffer-name)
                 ;; clean up the exported HTML
                 (org-apple-notes--replace-regex-in-buffer "<head>.*\\(\n.*\\)*</head>\\(\n*\\)?" "")
                 (org-apple-notes--replace-regex-in-buffer "<html>\\(\n*\\)?" "")
                 (org-apple-notes--replace-regex-in-buffer "</html>\\(\n*\\)?" "")
                 (org-apple-notes--replace-regex-in-buffer "<body>\\(\n*\\)?" "")
                 (org-apple-notes--replace-regex-in-buffer "</body>\\(\n*\\)?" "")
                 (org-apple-notes--replace-regex-in-buffer "\\(<.*\\)[[:space:]]+class=\"[[:alnum:]-_]+\"" "\\1")
                 (org-apple-notes--replace-regex-in-buffer "\\(<.*\\)[[:space:]]+id=\"[[:alnum:]-_]+\"" "\\1")
                 ;; clean up table newlines
                 (org-apple-notes--replace-regex-in-buffer "\n+<colgroup>" "\n<colgroup>")
                 (org-apple-notes--replace-regex-in-buffer "\n+<col[[:space:]*]>" "\n<col>")
                 (org-apple-notes--replace-regex-in-buffer "\n+<tr>" "\n<tr>")
                 ;; replace other newlines with <br> tags
                 (org-apple-notes--replace-regex-in-buffer "<div>\n*<p>" "<div><br><p>")
                 (org-apple-notes--replace-regex-in-buffer "\\(\n\\)\\(\n\\)\\{2\\}" "\n<br><br>")
                 (org-apple-notes--replace-regex-in-buffer "\\(\n\\)\\(\n\\)\\{1\\}" "\n<br>")
                 ;; write
                 (org-apple-notes--write-apple-note account folder note (buffer-string)))
            (kill-buffer tmp-buffer-name)))))))


;;; Footer

(provide 'org-apple-notes)
