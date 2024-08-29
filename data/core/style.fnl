(local common (require :core.common))
(local style {})

;; Size

(tset style :scrollbar-size (common.round (* 4 SCALE)))

;; Fonts

(tset style :font
    (renderer.font.load
        (.. EXEDIR "/data/fonts/ZeitungMonoProNerdFont-Regular.ttf")
        (* 14 SCALE)))

(tset style :big-font
    (renderer.font.load
        (.. EXEDIR "/data/fonts/ZeitungMonoProNerdFont-Regular.ttf")
        (* 24 SCALE)))

(tset style :icon-font
    (renderer.font.load
        (.. EXEDIR "/data/fonts/icons.ttf")
        (* 14 SCALE)))

(tset style :titlebar-font
    (renderer.font.load
        (.. EXEDIR "/data/fonts/icons.ttf")
        (* 20 SCALE)))

;; Colors

(tset style :titlebar-background [ (common.color "#111113") ])

style
