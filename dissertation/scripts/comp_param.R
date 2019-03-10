# PARAMETERS FOR COMPARISON

# contrasts for regressions

# target character: 2 edge cases
contrasts_sum_edge <- cbind("_robot_GM" = c(-1, 1))
contrasts_trt_edge <- cbind("_robot" = c(0, 1))

# target character: 21 diverse characters
contrasts_sum_dv21 <- contr.sum(21)
contrasts_trt_dv21 <- contr.treatment(21)

# target character: 9 diverse characters
contrasts_sum_dv09 <- contr.sum(9)
contrasts_trt_dv09 <- contr.treatment(9)


# age group: 2 age groups
contrasts_sum2_agegp <- cbind("_child_GM" = c(-1, 1))
contrasts_dum2_agegp <- cbind("_child" = c(0, 1))

# age group: 2 age groups
contrasts_sum3_agegp <- cbind("_old_GM" = c(-1, 0, 1),
                              "_yng_GM" = c(-1, 1, 0))
contrasts_dum3_agegp <- cbind("_old" = c(0, 0, 1),
                              "_yng" = c(0, 1, 0))
contrasts_cnt3_agegp <- cbind("_ch_ad" = c(-2, 1, 1),
                              "_yn_ol" = c(0, 1, -1))
