(local core {})
(local style (require :core.style))

(local config { :fps 60 })

(fn core.init []
    (local View (require :core.view))
    (local RootView (require :core.root-view))

    (local view (View))
    (local root-view (RootView))

    (root-view:update)

    (system.set-window-title (root-view:get-name))
)


(fn core.run []
    "Main Loop"

    (local system-begin-frame system.begin_frame)
    (local system-end-frame system.end_frame)

    (var time 0)

    (while true
        (system-begin-frame)

        (set core.frame-start (system.get-time))

        (core.step)

        (var elapsed (- (system.get-time) core.frame-start))
        (local frame (/ 1 60))

        (when (> frame elapsed)
            (local gc-duration (- time (math.floor time)))
            (if (<= gc-duration frame)
                (system.gc gc-duration)
            )

            (set elapsed (- (system.get-time) core.frame-start))
        )

        (var delta 0)
        (if (> frame elapsed)
            (do
                (system.sleep (- frame elapsed))
                (set delta frame)
            )
            (set delta elapsed)
        )

        (set time (+ time delta))
        (set core.fps (math.min config.fps (/ 1 delta)))

        (system-end-frame)
    )
)


(local renderer-begin-frame renderer.begin-frame)
(local renderer-end-frame renderer.end-frame)

(fn core.step []
    "Step of the Main Loop"

    (each [type a b c d e system.poll-event]
        (when (= type "quit")
            (os.exit)
        )
    )

    (let [(width height) (renderer.get-size)]

        (renderer-begin-frame)

        ;; Draw border
        (renderer.draw-rect 0 0 1 height style.titlebar-background)
        (renderer.draw-rect (- width 1) 0 1 height style.titlebar-background)
        (renderer.draw-rect 0 (- height 1) width 1 style.titlebar-background)

        (renderer-end-frame)
    )

    true
)

core
