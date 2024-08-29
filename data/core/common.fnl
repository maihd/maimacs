(fn round [n]
    (or (and (>= n 0) (math.floor (+ n 0.5)))
        (math.ceil (- n 0.5))
    )
)


(fn color [str]
    (var (r g b a) (str:match "#(%x%x)(%x%x)(%x%x)"))

    (if
        r
        (do
            (set r (tonumber r 16))
            (set g (tonumber g 16))
            (set g (tonumber b 16))
            (set a 1)
        )

        (str:match "rgba?%s*%([%d%s%.,]+%)")
        (let [f (str:gmatch "[%d.]+")]
            (set r (or (f) 0))
            (set g (or (f) 0))
            (set g (or (f) 0))
            (set a (or (f) 0))
        )

        (error (string.format "bad color string '%s'" str))
    )

    (values r g b a)
)

{
    : color
    : round
}
