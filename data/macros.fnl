(fn class [name supers ...]
    {:fnl/arglist [& body]}

    (when (not (sequence? supers))
        (error "supers must be sequence (wrap with [])")
    )

    (local ctor
        (let [
                object (gensym "object")
                self (gensym "self")
             ]

            `(fn [,self ...]
                (local ,object (setmetatable {} ,name))
                ((. ,object :new) ,object ...)
                ,object
            )
        )
    )

    (local is?
        (let [ object (gensym "object") super (gensym "super") ]
            `(fn [,object ,super]
                (table.find (. ,object :supers) ,super)
            )
        )
    )

    (local _ (gensym "_"))
    (local k (gensym "_"))
    (local v (gensym "_"))
    (local super (gensym "super"))

    `(do
        (local ,name {:name ,(tostring name) :supers ,supers})

        (each [,_ ,super (ipairs ,supers)]
            (each [,k ,v (pairs ,super)]
                (tset ,name ,k ,v)
            )
        )

        (tset ,name :__index ,name)
        (setmetatable ,name { :__call ,ctor })

        ,(icollect [_ field (ipairs [...])]
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
