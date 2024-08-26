(fn new-class [name supers]
    {:name name :supers supers}
)

(fn class [name supers & body]
    ; (when (not (sequence? supers))
    ;     (error "supers must be sequence (wrap with [])")
    ; )

    `(do
        (local ,name (new-class ,name ,supers))

        ,(icollect [_ field (ipairs body)]
            (if (list? field)
                (if (= (tostring (. field 1)) "fn")
                    `(tset ,name ,(tostring (. field 2)) ,field)
                    `(tset ,name ,(tostring (. field 1)) ,(. field 2))
                )
                (error "field must be in block")
            )
        )

        ,name
    )
)

{: class}
