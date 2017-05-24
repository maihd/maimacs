;;;
;;; MaiEmacs 2017, by MaiHD
;;;

(defconst emacs-temp-directory
  (expand-file-name (format "emacs%d" (user-uid))
		    temporary-file-directory))

(setq backup-directory-alist
      `((".*" . ,emacs-temp-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-temp-directory t)))
(setq auto-save-list-file-prefix
      emacs-temp-directory)
