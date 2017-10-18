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
    (load-modules '("autosave.el" "style.el"
		    "web-mode.el" "lua-mode.el"
		    "glsl-mode.el" "nasm-mode.el"
		    "maiblue-theme.el"))
    ;; loading slime
    (add-to-list 'load-path (expand-file-name "slime" maimacs-directory))
    (require 'slime-autoloads)
    (setq inferior-lisp-program "sbcl")
    ;; initializations
    (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.inl\\'" . c-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
    (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
    (add-to-list 'auto-mode-alist '("\\.s\\'" . nasm-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))


