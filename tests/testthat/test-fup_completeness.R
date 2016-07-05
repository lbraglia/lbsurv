context("fup_completeness")

test_that("fup completeness basic usage", {

    time   <- c(180, 12, 240, 250)
    status <- c(  0,  1,   0,   1)
    group  <- c("A", "A", "B", "B")
    res <- fup_completeness(time = time,
                            status = status,
                            cutoff = 200,
                            strata = group)
    right <- structure(list(time = 200,
                            overall = 0.967320261437909,
                            A = 0.905660377358491,
                            B = 1),
                       .Names = c("time", "overall", "A", "B"),
                       row.names = 1L, class = "data.frame")
    
    expect_equal(res, right)
    
})

