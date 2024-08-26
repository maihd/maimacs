(import-macros {: class} :base.class)

(class View []
    (fn new [self]
        (tset self :name "---")
    )


    (fn get-name [self]
        (. self :name)
    )


    (fn update []
        ;; no-op
    )


    (fn draw []
        ;; no-op
    )
)
