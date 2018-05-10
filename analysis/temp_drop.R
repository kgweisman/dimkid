chosen_rot <- "varimax"
# chosen_rot <- "oblimin"

# chosen_exclude <- c("awareness", "depth", "embarrassment", 
#                     "guilt", "pride", "temperature")
# chosen_exclude <- c("awareness", "embarrassment", 
#                     "guilt", "pride")
chosen_exclude <- c("embarrassment", "guilt", "pride")

d_young_wide[!names(d_young_wide) %in% chosen_exclude] %>%
  fa(nfactors = floor(ncol(.)/3), rotate = "none") %>%
  fa.sort()

d_young_wide[!names(d_young_wide) %in% chosen_exclude] %>%
  fa(nfactors = 4, rotate = chosen_rot) %>%
  fa.sort()

d_young_wide[!names(d_young_wide) %in% chosen_exclude] %>%
  fa(nfactors = 3, rotate = chosen_rot) %>%
  fa.sort()

d_young_wide[!names(d_young_wide) %in% chosen_exclude] %>%
  fa(nfactors = 2, rotate = chosen_rot) %>%
  fa.sort()

d_young_wide[!names(d_young_wide) %in% chosen_exclude] %>%
  fa(nfactors = 1, rotate = chosen_rot) %>%
  fa.sort()
