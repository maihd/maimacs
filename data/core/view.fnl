(import-macros {: class} :macros)

(local style (require :core.style))
(local common (require :core.common))

(class View []
    (fn new [self]
        (tset self :name "---")

        (tset self :position { :x 0 :y 0 })
        (tset self :size { :x 0 :y 0 })
        (tset self :scroll { :x 0 :y 0 })
        (tset self :cursor :arrow)
        (tset self :focusable true)
        (tset self :resizable false)
        (tset self :scrollable false)
    )


    (fn move-towards [self t k dest rate]
        (if (~= (type t) :table)
            (self:move-towards self t k dest rate)

            (let [val (. t k)]
                (if (< (math.abs (- val dest)) 0.5)
                    (tset t k dest)
                    (tset t k (common.lerp val dest (or rate 0.5))
                )
            )
        )
    )


    (fn try-close [self do-close]
        (do-close)
    )


    (fn get-name [self]
        (. self :name)
    )


    (fn get-scrollable-size [self]
        math.huge
    )


    (fn get-scrollable-rect [self]
        (local sz (self:get-scrollable-size))

        (if (or (<= sz self.size.y)
                (= sz math.huge))
            (values 0 0 0 0)
            (let [h (math.max 20 (/ (* self.size.y self.size.y) sz))]
                (values
                    (- (+ self.position.x self.size.x) style.scrollbar-size)
                    (+ self.position.y (/ (* self.scroll.y (- self.size.y h)) (- sz self.size.y))))
                    style.scrollbar_size
                    h
                )
            )
        )
    )


    (fn update []
        ;; no-op
    )


    (fn draw []
        ;; no-op
    )
)
