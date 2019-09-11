context("Function")

library(annotatr)


test_that("functions without annotations are parsed correctly", {

    expect_same(
        parse_text(
            "function(f, x, y) {",
            "    f(x, y)",
            "}"
        ),
        expression(
            function(f, x, y) {
                f(x, y)
            }
        )
    )

    expect_same(
        parse_text(
            "function() {",
            "    1 + 2",
            "}"
        ),
        expression(
            function() {
                1 + 2
            }
        )
    )

})


test_that("functions with header annotations are parsed correctly", {

    expect_same(
        parse_text(
            "@:ann1",
            "function(f, x, y) {",
            "    f(x + y)",
            "}"
        ),
        expression(
            `@:`(expression(ann1),
                 function(f, x, y) {
                     f(x + y)
                 })
        )
    )

    expect_same(
        parse_text(
            "@:ann1 function(f, x, y) {",
            "    f(x + y)",
            "}"
        ),
        expression(
            `@:`(expression(ann1),
                 function(f, x, y) {
                     f(x + y)
                 })
        )
    )

    expect_same(
        parse_text(
            "@:ann1",
            "@:ann2 function(f, x, y) {",
            "    f(x + y)",
            "}"
        ),
        expression(
            `@:`(expression(ann1, ann2),
                 function(f, x, y) {
                     f(x + y)
                 })
        )
    )

    expect_same(
        parse_text(
            "@:ann1",
            "@:ann2",
            "function(f, x, y) {",
            "    f(x + y)",
            "}"
        ),
        expression(
            `@:`(expression(ann1, ann2),
                 function(f, x, y) {
                     f(x + y)
                 })
        )
    )

    expect_same(
        parse_text(
            "@:ann1 @:ann2",
            "function(f, x, y) {",
            "    f(x + y)",
            "}"
        ),
        expression(
            `@:`(expression(ann1, ann2),
                 function(f, x, y) {
                     f(x + y)
                 })
        )
    )

    expect_same(
        parse_text(
            "@:ann1",
            "@:ann2",
            "@:ann3 @:ann4  function(f, x, y) {",
            "    f(x + y)",
            "}"
        ),
        expression(
            `@:`(expression(ann1, ann2, ann3, ann4),
                 function(f, x, y) {
                     f(x + y)
                 })
        )
    )

    expect_same(
        parse_text(
            "@:ann1",
            "@:ann2",
            "@:ann3 @:ann4 function() {",
            "    1 + 2",
            "}"
        ),
        expression(
            `@:`(expression(ann1, ann2, ann3, ann4),
                 function() {
                     1 + 2
                 })
        )
    )

})


test_that("functions with parameter annotations are parsed correctly", {

    expect_same(
        parse_text(
            "function(@:ann11 f, x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(@:ann11 @:ann12 f, x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11, ann12))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(@:ann11",
            "         @:ann12 f, x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11, ann12))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(@:ann11",
            "         @:ann12 @:ann13 f, x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11, ann12, ann13))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(@:ann11 @:ann12",
            "         @:ann13 @:ann14 f, x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11, ann12, ann13, ann14))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(f, @:ann21 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(x = expression(ann21))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(f, @:ann21 @:ann22 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(x = expression(ann21, ann22))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(f, @:ann21",
            "            @:ann22 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(x = expression(ann21, ann22))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(f, @:ann21",
            "            @:ann22 @:ann23 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(x = expression(ann21, ann22, ann23))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(f, @:ann21 @:ann22",
            "            @:ann23 @:ann24 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(x = expression(ann21, ann22, ann23, ann24))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

        expect_same(
        parse_text(
            "function(@:ann11 f, @:ann21 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11),
                                                 x = expression(ann21))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(@:ann11 @:ann12 f, @:ann21 @:ann22 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11, ann12),
                                                 x = expression(ann21, ann22))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(@:ann11",
            "         @:ann12 f,",
            "         @:ann21",
            "         @:ann22 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11, ann12),
                                                 x = expression(ann21, ann22))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(@:ann11",
            "         @:ann12 @:ann13 f,",
            "         @:ann21",
            "         @:ann22 @:ann23 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11, ann12, ann13),
                                                 x = expression(ann21, ann22, ann23))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

    expect_same(
        parse_text(
            "function(@:ann11 @:ann12",
            "         @:ann13 @:ann14 f,",
            "         @:ann21 @:ann22",
            "         @:ann23 @:ann24 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11, ann12, ann13, ann14),
                                                 x = expression(ann21, ann22, ann23, ann24))),
                 function(f, x) {
                     f(x)
                 })
        )
    )
})


test_that("functions with body annotations are parsed correctly", {

    expect_same(
        parse_text(
            "function(f, x) @:ann1",
            "{",
            "    f(x)",
            "}"
        ),
        expression(
            function(f, x)
                `@:`(expression(ann1),
                {
                    f(x)
                })
        )
    )

    expect_same(
        parse_text(
            "function(f, x) @:ann1 @:ann2",
            "{",
            "    f(x)",
            "}"
        ),
        expression(
            function(f, x)
                `@:`(expression(ann1, ann2),
                {
                    f(x)
                })
        )
    )

    expect_same(
        parse_text(
            "function(f, x) @:ann1",
            "@:ann2 {",
            "    f(x)",
            "}"
        ),
        expression(
            function(f, x)
                `@:`(expression(ann1, ann2),
                {
                    f(x)
                })
        )
    )

    expect_same(
        parse_text(
            "function(f, x) @:ann1 @:ann2",
            "@:ann3 {",
            "    f(x)",
            "}"
        ),
        expression(
            function(f, x)
                `@:`(expression(ann1, ann2, ann3),
                {
                    f(x)
                })
        )
    )

    expect_same(
        parse_text(
            "function(f, x) @:ann1 @:ann2",
            "@:ann3 @:ann4 {",
            "    f(x)",
            "}"
        ),
        expression(
            function(f, x)
                `@:`(expression(ann1, ann2, ann3, ann4),
                {
                    f(x)
                })
        )
    )

    expect_same(
        parse_text(
            "function() @:ann1 @:ann2",
            "@:ann3 @:ann4 {",
            "    1 + 2",
            "}"
        ),
        expression(
            function()
                `@:`(expression(ann1, ann2, ann3, ann4),
                {
                    1 + 2
                })
        )
    )
})


test_that("functions with header and parameter annotations are parsed correctly", {

    expect_same(
        parse_text(
            "@:ann1",
            "@:ann2",
            "@:ann3 @:ann4 function(@:ann11 @:ann12",
            "                       @:ann13 @:ann14 f,",
            "                       @:ann21 @:ann22",
            "                       @:ann23 @:ann24 x) {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(ann1, ann2, ann3, ann4,
                            formals = expression(f = expression(ann11, ann12, ann13, ann14),
                                                 x = expression(ann21, ann22, ann23, ann24))),
                 function(f, x) {
                     f(x)
                 })
        )
    )

})


test_that("functions with header and body annotations are parsed correctly", {

    expect_same(
        parse_text(
            "@:ann1",
            "@:ann2",
            "@:ann3 @:ann4 function(f, x) @:ann5 @:ann6",
            "@:ann7 @:ann8 {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(ann1, ann2, ann3, ann4),
                 function(f, x)
                     `@:`(expression(ann5, ann6, ann7, ann8), {
                         f(x)
                     }))
        )
    )

    expect_same(
        parse_text(
            "@:ann1",
            "@:ann2",
            "@:ann3 @:ann4 function() @:ann5 @:ann6",
            "@:ann7 @:ann8 {",
            "    1 + 2",
            "}"
        ),
        expression(
            `@:`(expression(ann1, ann2, ann3, ann4),
                 function()
                     `@:`(expression(ann5, ann6, ann7, ann8), {
                         1 + 2
                     }))
        )
    )
})


test_that("functions with parameter and body annotations are parsed correctly", {

    expect_same(
        parse_text(
            "function(@:ann11 @:ann12",
            "         @:ann13 @:ann14 f,",
            "         @:ann21 @:ann22",
            "         @:ann23 @:ann24 x) @:ann1 @:ann2",
            "@:ann3 @:ann4 {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(formals = expression(f = expression(ann11, ann12, ann13, ann14),
                                                 x = expression(ann21, ann22, ann23, ann24))),
                 function(f, x)
                     `@:`(expression(ann1, ann2, ann3, ann4), {
                         f(x)
                     }))
        )
    )
})


test_that("functions with header, parameter and body annotations are parsed correctly", {

    expect_same(
        parse_text(
            "@:ann1",
            "@:ann2",
            "@:ann3 @:ann4 function(@:ann11 @:ann12",
            "                       @:ann13 @:ann14 f,",
            "                       @:ann21 @:ann22",
            "                       @:ann23 @:ann24 x) @:ann5 @:ann6",
            "                                          @:ann7 @:ann8 {",
            "    f(x)",
            "}"
        ),
        expression(
            `@:`(expression(ann1, ann2, ann3, ann4,
                            formals = expression(f = expression(ann11, ann12, ann13, ann14),
                                                 x = expression(ann21, ann22, ann23, ann24))),
                 function(f, x)
                     `@:`(expression(ann5, ann6, ann7, ann8), {
                         f(x)
                     }))
        )
    )
})
