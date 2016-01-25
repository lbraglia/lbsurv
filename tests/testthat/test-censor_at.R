context("censor_at")

ctimes <- c(160, 150, 125, 75)

test_that("censor_at basic usage", {
    time   <- c(100,150, 200)
    status <- c(  0,  1,  NA)
    res <- censor_at(time = time, status = status, censoring_time = ctimes)
    right <- data.frame('time_c75'    = c( 75,  75, NA),
                        'status_c75'  = c(  0,   0, NA),
                        'time_c125'   = c(100, 125, NA),
                        'status_c125' = c(  0,   0, NA),
                        'time_c150'   = c(100, 150, NA),
                        'status_c150' = c(  0,   1, NA),
                        'time_c160'   = c(100, 150, NA),
                        'status_c160' = c(  0,   1, NA))
    expect_equal(res, right)
})

