# PARAMETERS FOR SCORING

# which loadings to count
chosen_count <- "raw" # raw loadings - REPORTED
# chosen_count <- "absolute" # absolute value of loadings

# whether to make scales of equal length
chosen_trim <- TRUE # yes, trim scales to equal length - REPORTED
# chosen_trim <- FALSE # no, allow scales to vary in length

# number of items to keep per scale
# chosen_n_keep <- NA # as many as possible
chosen_n_keep <- 6 # up to 6-item scale - REPORTED
# chosen_n_keep <- 5 # up to 5-item scale
# chosen_n_keep <- 4 # up to 4-item scale (and so on...)

# minimum factor loading (on dominant factor)
chosen_min_loading <- NA # none - REPORTED
# chosen_min_loading <- 0.6 # 0.6
# chosen_min_loading <- 0.4 # 0.4 (and so on...)

# maximum cross-loading to allow
chosen_max_cross <- NA # anything - REPORTED
# chosen_max_cross <- 0.3 # 0.3
# chosen_max_cross <- 0.5 # 0.5 (and so on...)
