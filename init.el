;;;
;;; MaiMacs 2017 - 2018, by MaiHD
;;;

(defconst maimacs/version "0.1.1"
  "MaiMacs version.")

(defconst maimacs/emacs-min-version "24.0.0"
  "Emacs min version that is supported by MaiMacs.")

(defconst maimacs/source-files
  '("themes/maiblue-three-theme.el"
    
    "common/nlinum.el"
    "common/autosave.el"
    "common/frame-style.el"
    "common/key-bindings.el")
  "Maimacs' source files")

(defconst maimacs/mode-files
  '((c-mode . "mode/c-mode.el")
    (web-mode . "mode/web-mode.el")
    (lua-mode . "mode/lua-mode.el")
    (glsl-mode . "mode/glsl-mode.el")
    (nasm-mode . "mode/nasm-mode.el")
    (markdown-mode . "mode/markdown-mode.el"))
  "Maimacs' source files")

(if (version< emacs-version maimacs/emacs-min-version)
    (error (concat "Your Emacs version (%s) is too old. "
		           "Maimacs require Emacs version %s or above.")
	       emacs-version
	       maimacs/emacs-min-version)
  (defun maimacs-init (maimacs-directory)
    "Initialize maimacs"
    
    ;; Encoding
    (set-language-environment "UTF-8")
    
    ;; Hide welcome screen
    (setq inhibit-splash-screen t)
    (setq inhibit-startup-message t)
    
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

    ;; Autoload for mode
    (defun autoload-modes (modes-list)
      "Autoload modes in maimacs-directory"
      (when modes-list
        (letrec ((pair (car modes-list))
                 (mode (car pair))
                 (file (cdr pair)))
          (autoload mode (expand-file-name file maimacs-directory)))
        (autoload-modes (cdr modes-list))))
    (autoload-modes  maimacs/mode-files)
    
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
    (add-to-list 'auto-mode-alist '("\\.md\\'"   . markdown-mode))

    ;; Initialize interpreters
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

    ;; Change main mode to c-mode
    (setq initial-major-mode 'c-mode)
    (setq initial-scratch-message "")))

;; @endfile: init.el
