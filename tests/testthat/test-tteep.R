context("tteep")

test_that("tteep is ok with numerics", {

    test_data <- data.frame(
        start_date = c(0,  0,  0,  0, NA,  0, NA),
        prog_date  = c(3,  3, NA, NA, NA, NA, NA),
        death_date = c(6, NA,  6, NA, NA, NA, NA),
        last_fup   = c(6, 12,  6, 12, 12, NA, NA)
    )
    
    right_answer <- structure(list(
        os_time    = c(6, 12, 6, 12, NA, NA, NA),
        os_status  = c(1,  0, 1,  0,  0,  0,  0),
        pfs_time   = c(3,  3, 6, 12, NA, NA, NA),
        pfs_status = c(1,  1, 1,  0,  0,  0,  0),
        ttp_time   = c(3,  3, 6, 12, NA, NA, NA),
        ttp_status = c(1,  1, 0,  0,  0,  0,  0)),
        .Names     = c("os_time", "os_status",
                       "pfs_time", "pfs_status", 
                       "ttp_time", "ttp_status"),
        row.names  = c(NA, -7),
        class      = "data.frame")
    
    with(test_data,
         expect_equal(tteep(start_date, prog_date, death_date, last_fup),
                      right_answer))
})
