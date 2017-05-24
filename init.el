;;;
;;; MaiMacs 2017, by MaiHD
;;;

(defconst maimacs/version "0.0.1" "MaiMacs version.")
(defconst maimacs/emacs-min-version "24.0.0" "Emacs min version that is supported by MaiMacs.")

(if (version< emacs-version maimacs/emacs-min-version)
    (error (concat "Your Emacs version (%s) is too old. "
		   "Maimacs require Emacs version %s or above.")
	   emacs-version
	   maimacs/emacs-min-version)
  (defun maimacs-init (maimacs-directory)
    (add-to-list 'load-path maimacs-directory)
    ;; loading modules
    (defun load-modules (modules-list)
      (when modules-list
	(load (expand-file-name (car modules-list)
				maimacs-directory))
	(load-modules (cdr modules-list))))
    (load-modules '("autosave.el" "style.el"))))


