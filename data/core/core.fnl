(local core {})

(local config { :fps 60 })

(fn core.init []
    (local View (require :core.view))
    (local RootView (require :core.root-view))

    (local view (View))
    (local root-view (RootView))

    (root-view:update)

    (system.set_window_title (root-view:get-name))
)

(fn core.run []
    "Main Loop"

    (local system/begin-frame system.begin_frame)
    (local system/end-frame system.end_frame)

    (var time 0)

    (while true
        (system/begin-frame)

        (set core.frame-start (system.get_time))

        (core.step)

        (var elapsed (- (system.get_time) core.frame-start))
        (local frame (/ 1 60))

        (when (> frame elapsed)
            (if (<= (- time (math.floor time))
                    frame)
                (collectgarbage "collect")
            )

            (set elapsed (- (system.get_time) core.frame-start))
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

        (system/end-frame)
    )
)


(fn core.step []
    (each [type a b c d e system.poll_event]
        (when (= type "quit")
            (os.exit)
        )
    )

    (renderer.begin_frame)
    (renderer.end_frame)

    true
)

core
