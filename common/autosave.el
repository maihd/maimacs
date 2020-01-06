;;;
;;; MaiEmacs 2017 - 2020, by MaiHD
;;;

(defconst emacs-temp-directory
  (expand-file-name (format ".emacs.temp.d.%d" (user-uid))
  		    temporary-file-directory))
  
(setq backup-directory-alist
      `(("" . ,emacs-temp-directory)))
(setq auto-save-file-name-transforms
      `(("" ,emacs-temp-directory t)))
(setq auto-save-list-file-prefix
      emacs-temp-directory)

;; Disable auto save, in Windows above code wont work
;; @note: still have .#* auto-save files
(cond
 ((string-equal system-type "windows-nt")
  (setq make-backup-files nil)
  (setq auto-save-default nil)))
