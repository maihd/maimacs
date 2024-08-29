;;
;; LiteFx renderer module bindings to fennel
;; The main purpose is to have good name conventions (kebab-case instead of snake_case)
;; Links: https://www.cliki.net/Naming+conventions
;;

(tset _G :old-renderer renderer)

(tset _G :renderer
    {
        :begin-frame renderer.begin_frame
        :end-frame renderer.end_frame

        :show-debug renderer.show_debug
        :get-size renderer.get_size

        :set-clip-rect renderer.get_clip_rect
        :draw-rect renderer.draw_rect
        :draw-text renderer.draw_text
    }
)
