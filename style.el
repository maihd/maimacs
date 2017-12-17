;;;
;;; MaiMacs 2017, by MaiHD
;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)

;; setting fonts
(cond
 ((find-font (font-spec :name "Hack"))
  (set-frame-font "Hack")))

(set-face-attribute 'default nil :height 120)
;(set-face-attribute 'fringe nil :background "#2E2920" :foreground "#2E2920")
