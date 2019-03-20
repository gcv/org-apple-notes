;;; org-apple-notes-sync.el --- Org to Apple Notes synchronizer  -*- lexical-binding: t; -*-

;;; Requirements


;;; Customization

(defgroup org-apple-notes-sync nil
  "Customization options for org-apple-notes-sync."
  :group 'external)

(defcustom org-apple-notes-sync-completing-read-fn #'completing-read
  "ido users should set this to ido-completing-read.
Helm users should set this to helm-comp-read.
Ivy users should set this to ivy-completing-read."
  :group 'org-apple-notes-sync
  :type 'function)


;;; Variables


;;; Keymaps


;;; Commands

;; ;;;###autoload
;; (defun package-name-command (args)
;;   "Frobnicate the flange."
;;   (interactive)
;;   (package-name--something)
;;   (bar))

(defun org-apple-notes-sync-write-buffer-to-apple-note ()
  (interactive)
  (if (not (eq 'org-mode major-mode))
      (message "not in Org mode")
    (seq-let [account folder note] (org-apple-notes-sync--select-apple-note)
      (message note)
      )))


;;; Support

(setq org-apple-notes-sync--applescript-tmpl-base "
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

(setq org-apple-notes-sync--applescript-tmpl-list-notes "
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

(setq org-apple-notes-sync--applescript-tmpl-read-note "
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

(setq org-apple-notes-sync--applescript-tmpl-write-note "
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

(defun org-apple-notes-sync--applescript (str)
  (declare (indent 0))
  (let ((scpt (format "%s\n%s" org-apple-notes-sync--applescript-tmpl-base str)))
    (do-applescript scpt)))

(defun org-apple-notes-sync--list-apple-notes ()
  "Returns all Notes documents."
  (let* ((notes-raw (org-apple-notes-sync--applescript
                      org-apple-notes-sync--applescript-tmpl-list-notes))
         (notes (json-read-from-string notes-raw)))
    notes))

(defun org-apple-notes-sync--select-apple-note ()
  (let ((notes (org-apple-notes-sync--list-apple-notes))
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
    (let ((selected-note (funcall org-apple-notes-sync-completing-read-fn
                                  "Select Apple note: "
                                  (sort display-names #'string-lessp))))
      (gethash selected-note notes-for-completing-read))))

(defun org-apple-notes-sync--read-apple-note (account folder note)
  "Read a Notes document."
  (org-apple-notes-sync--applescript
    (format org-apple-notes-sync--applescript-tmpl-read-note
            account folder note)))

(defun org-apple-notes-sync--write-apple-note (account folder note new-contents)
  "Destructively write a Notes document."
  (org-apple-notes-sync--applescript
    (format org-apple-notes-sync--applescript-tmpl-write-note
            account folder note new-contents)))


;;; Footer

;; (provide 'org-apple-notes-sync)
