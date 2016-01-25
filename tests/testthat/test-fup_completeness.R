context("fup_completeness")

test_that("fup completeness basic usage", {

    time   <- c(180, 12, 240, 250)
    status <- c(  0,  1,   0,   1)
    group  <- c("A", "A", "B", "B")
    res <- fup_completeness(time = time,
                            status = status,
                            cutoff = 200,
                            strata = group)
    right <- list('globalC' = 0.967320261437909,
                  'individualC' = c(0.9, 1, 1, 1),
                  'strataC' = data.frame('Group.1'       = c("A", "B"),
                                         'time'          = c(192, 400),
                                         'potential_fup' = c(212, 400),
                                         'strataC' = c(0.905660377358491, 1)))
    
    expect_equal(res, right)
    
})

