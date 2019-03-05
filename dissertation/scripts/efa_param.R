# PARAMETERS FOR EFA

# what correlation to use
chosen_cor <- "cor" # pearson - REPORTED
# chosen_cor <- "poly" # polychoric

# what rotation to use
chosen_rot <- "varimax" # orthogonal factors - REPORTED
# chosen_rot <- "oblimin" # correlated factors

# what factoring method to use
chosen_fm <- "minres" # minimum residuals - REPORTED
# chosen_fm <- "ols" # ordinary least squares using empirical first derivative
# chosen_fm <- "wls" # weighted least squares
# chosen_fm <- "gls" # generalized weighted least squares
# chosen_fm <- "pa" # principal factors
# chosen_fm <- "ml" # maximum likelihood
# chosen_fm <- "minchi" # minimize ss-weighted chisq
# chosen_fm <- "minrank" # minimum rank
# chosen_fm <- "old.min" # minres < April 2017
# chosen_fm <- "alpha" # alpha fa (Kaiser & Coffey, 1965)

# what scoring method to use
chosen_scores <- "tenBerge" # correlation-preserving - REPORTED
# chosen_scores <- "regression" # regression approach