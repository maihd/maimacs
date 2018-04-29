;;;
;;; MaiMacs 2017 - 2018, by MaiHD
;;;

(menu-bar-mode    -1)
(tool-bar-mode    -1)
(scroll-bar-mode  -1)
(global-linum-mode 1)

;; setting fonts
(cond
 ((find-font (font-spec :name "Adale Mono"))
  (set-frame-font "Adale Mono"))
 ((find-font (font-spec :name "Consolas"))
  (set-frame-font "Consolas"))
 ((find-font (font-spec :name "Hack"))
  (set-frame-font "Hack"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata"))
 ((find-font (font-spec :name "Anonymous Pro"))
  (set-frame-font "Anonymous Pro")))

(set-face-attribute 'default nil :height 120)
;(set-face-attribute 'fringe nil :background "#2E2920" :foreground "#2E2920")
