;;;
;;; MaiMacs 2017 - 2018, by MaiHD
;;;

(defconst maimacs/version "0.1.0"
  "MaiMacs version.")

(defconst maimacs/emacs-min-version "24.0.0"
  "Emacs min version that is supported by MaiMacs.")

(defconst maimacs/source-files
  '("c-mode.el"
    "web-mode.el"
    "lua-mode.el"
    "glsl-mode.el"
    "nasm-mode.el"
    "unity-shader-mode.el"
    
    "maibluetwo-theme.el"

    "nlinum.el"
    "maiterm.el"
    "autosave.el"
    "frame-style.el"
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
    
    ;; Initialize files' auto mode detection
    (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.h\\'"    .  c++-mode))
    (add-to-list 'auto-mode-alist '("\\.inl\\'"  .  c++-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'"  .  web-mode))
    (add-to-list 'auto-mode-alist '("\\.lua\\'"  .  lua-mode))
    (add-to-list 'auto-mode-alist '("\\.asm\\'"  . nasm-mode))
    (add-to-list 'auto-mode-alist '("\\.s\\'"    . nasm-mode))

    ;; Initialize interpreters
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))

;; @endfile: init.el
