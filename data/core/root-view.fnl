(import-macros {: class} :macros)

(local View (require :core.view))

(class Node []

)

(class RootView [View]
    (fn new [self]

    )

    (fn get-name [self]
        "Root-View"
    )
)
