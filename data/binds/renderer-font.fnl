;;
;; LiteFx renderer-font module bindings to fennel
;; The main purpose is to have good name conventions (kebab-case instead of snake_case)
;; Links: https://www.cliki.net/Naming+conventions
;;

(local old-font _G.old-renderer.font)

(local Font
    {
        :__gc old-font.__gc
        :load old-font.load
        :set-tab-width old-font.set_tab_width
        :get-width old-font.get_width
        :get-height old-font.get_height
    }
)

(tset Font :__index Font)
(tset renderer :font Font)
