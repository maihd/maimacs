;;;
;;; MaiMacs 2017 - 2018, by MaiHD
;;;

(defconst maimacs/version "0.1.0"
  "MaiMacs version.")

(defconst maimacs/emacs-min-version "24.0.0"
  "Emacs min version that is supported by MaiMacs.")

(defconst maimacs/source-files
  '("autosave.el"
    "style.el"
    "nlinum.el"
    "c-mode.el"
    "maiterm.el"
    "web-mode.el"
    "lua-mode.el"
    "glsl-mode.el"
    "nasm-mode.el"
    "csharp-mode.el" 
    ;;"maiblue-theme.el"
    "maibluetwo-theme.el"
    "statusline.el"
    )
  "Maimacs' source files")
  
(if (version< emacs-version maimacs/emacs-min-version)
    (error (concat "Your Emacs version (%s) is too old. "
		   "Maimacs require Emacs version %s or above.")
	   emacs-version
	   maimacs/emacs-min-version)
  (defun maimacs-init (maimacs-directory)
    "Initialize maimacs"

    ;; Load path 
    (add-to-list 'load-path maimacs-directory)
    
    ;; Loading modules function
    (defun load-modules (modules-list)
      "Loading modules in maimacs-directory"
      (when modules-list
	(load (expand-file-name (car modules-list)
				maimacs-directory))
	(load-modules (cdr modules-list))))
    (load-modules maimacs/source-files)
    
    ;; Preset `nlinum-format' for minimum width.
    (defun maimacs-nlinum-mode-hook ()
      (when nlinum-mode
	(setq-local nlinum-format
		    (concat "%"
			    (number-to-string
			     ;; Guesstimate number of buffer lines.
			     (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
			    "d"))))
    (add-hook 'nlinum-mode-hook #'maimacs-nlinum-mode-hook)
    (set-face-foreground 'linum
			 ;;"#6f95ab") ;; maiblue-theme foreground color
			 "#7695b0")   ;; maibluetwo-theme foreground color
    (defun initialize-nlinum (&optional frame)
      (require 'nlinum)
      (add-hook 'prog-mode-hook 'nlinum-mode))
    (when (daemonp)
      (add-hook 'window-setup-hook 'initialize-nlinum)
      (defadvice make-frame (around toggle-nlinum-mode compile activate)
	(nlinum-mode -1) ad-do-it (nlinum-mode 1)))

    ;; Initialize files' auto mode detection
    (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.inl\\'"  .    c-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'"  .  web-mode))
    (add-to-list 'auto-mode-alist '("\\.lua\\'"  .  lua-mode))
    (add-to-list 'auto-mode-alist '("\\.asm\\'"  . nasm-mode))
    (add-to-list 'auto-mode-alist '("\\.s\\'"    . nasm-mode))

    ;; Initialize interpreters
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))


