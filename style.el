;;;
;;; MaiMacs 2017 - 2018, by MaiHD
;;;

(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(set-fringe-mode    0)
(global-nlinum-mode 1)

;; frame settings
(set-frame-width (selected-frame) 88)
(setq frame-title-format
      '((:eval (let ((name (buffer-file-name)))
		 (or name "%b")))
	" - MaiMacs"))

;; setting fonts
(cond
 ((find-font (font-spec :name "Adale Mono"))
  (set-frame-font "Adale Mono"))
 ((find-font (font-spec :name "Hack"))
  (set-frame-font "Hack"))
 ((find-font (font-spec :name "Consolas"))
  (set-frame-font "Consolas"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata"))
 ((find-font (font-spec :name "Anonymous Pro"))
  (set-frame-font "Anonymous Pro")))

;; font size
(set-face-attribute 'default nil :height 110)
