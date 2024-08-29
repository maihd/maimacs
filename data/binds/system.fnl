;;
;; LiteFx system module bindings to fennel
;; The main purpose is to have good name conventions (kebab-case instead of snake_case)
;; Links: https://www.cliki.net/Naming+conventions
;;

(set system.set-window-title system.set_window_title)
(set system.get-time system.get_time)
(set system.poll-event system.poll_event)

;; @note(maihd): should be replace with native function
(fn system.gc [duration]
    (collectgarbage "collect")
)
