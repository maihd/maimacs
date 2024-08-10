;;;
;;; MaiMacs 2017 - 2024, by MaiHD
;;;

;; Version checking

(defconst maimacs/version "0.1.1"
  "MaiMacs version.")

(defconst maimacs/emacs-min-version "24.0.0"
  "Emacs min version that is supported by MaiMacs.")

(defconst maimacs/emacs-max-version "24.99.99"
  "Emacs min version that is supported by MaiMacs.")

;; (cond
;;  ((version< emacs-version maimacs/emacs-min-version)
;;   (error (concat "Your Emacs version (%s) is too old. "
;;                  "Maimacs require Emacs version %s or above.")
;;          emacs-version
;;          maimacs/emacs-min-version))
;;  ((not (version< emacs-version maimacs/emacs-max-version))
;;   (error (concat "Your Emacs version (%s) is too new. "
;;                  "Maimacs require Emacs version %s and between.")
;;          emacs-version
;;          maimacs/emacs-max-version)))

;; Initialization

(defconst maimacs/source-files
  '("themes/atom-one-dark-theme.el"
    
    "common/nlinum.el"
    "common/autosave.el"
    "common/key-bindings.el"

    "common/all-the-icons-faces.el"
    "common/all-the-icons.el"
    "common/lambda-line.el"
    "common/frame-style.el")
  "Maimacs' source files")

(defconst maimacs/packages-directory
  "packages"
  "Maimacs' packages directory")

(defconst maimacs/packages
  '("dash-2.19.1"
    "expand-region-1.0.0"
    "multicursors-1.4.0"

    "boon-1.3")
  "Maimacs' packages")

(defconst maimacs/mode-files
  '((c-mode . "mode/c-mode.el")
    (web-mode . "mode/web-mode.el")
    (lua-mode . "mode/lua-mode.el")
    (urn-mode . "mode/urn-mode.el")
    (glsl-mode . "mode/glsl-mode.el")
    (nasm-mode . "mode/nasm-mode.el")
    (fennel-mode . "mode/fennel-mode.el")
    (markdown-mode . "mode/markdown-mode.el"))
  "Maimacs' source files")

(defun maimacs-init (maimacs-directory)
  "Initialize maimacs"
  
  ;; Encoding and line ending
  (set-language-environment "UTF-8")
  (set-buffer-file-coding-system 'unix t)
  (setq-default coding-system 'utf-8)
  (setq-default buffer-file-coding-sytem 'unix)
  
  ;; Hide welcome screen
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-message t)

  ;; Loading packages
  (defun load-packages (packages-list)
    "Loading packages in maimacs-directory"
    (when packages-list
      (add-to-list 'load-path (expand-file-name (car packages-list) (expand-file-name maimacs/packages-directory maimacs-directory)))
      (load-packages (cdr packages-list))))
  (load-packages maimacs/packages)
  
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
  (autoload-modes maimacs/mode-files)

  ;; Modal editting
  (cua-mode t)
    (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
    (transient-mark-mode 1) ;; No region when it is not highlighted
    (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
  
  ;; Initialize files' auto mode detection
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'"    .  c++-mode))
  (add-to-list 'auto-mode-alist '("\\.inl\\'"  .  c++-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'"  .  web-mode))
  (add-to-list 'auto-mode-alist '("\\.lua\\'"  .  lua-mode))
  (add-to-list 'auto-mode-alist '("\\.urn\\'"  .  urn-mode))
  (add-to-list 'auto-mode-alist '("\\.asm\\'"  . nasm-mode))
  (add-to-list 'auto-mode-alist '("\\.s\\'"    . nasm-mode))
  (add-to-list 'auto-mode-alist '("\\.fnl\\'"  . fennel-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'"   . markdown-mode))

  ;; Initialize interpreters
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

  ;; Change main mode to c-mode
  (setq initial-major-mode 'c-mode)
  (setq initial-scratch-message "")
  
  ;; Load theme
  (load-theme 'atom-one-dark t)
  
  ;; Change mode-line
  (maimacs/mode-line))

;; @endfile: maimacs/init.el
