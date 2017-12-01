library(tsfa)

d_slide_cov <- d_slide_all_complete %>%
  mutate(age = scale(age)) %>%
  select(-character) %>%
  cov()

# temp <- estFAmodel(d_slide_cov, n.obs = 250, p = 3, rotation = "oblimin")
temp <- estTSF.ML(d_slide_all_complete %>%
                    group_by(age) %>%
                    top_n(1) %>%
                    ungroup() %>%
                    mutate(character = factor(character,
                                              levels = c("computer", "robot",
                                                         "doll", "teddy_bear",
                                                         "beetle", "bird",
                                                         "mouse", "goat",
                                                         "elephant"))) %>%
                    arrange(character, age) %>%
                    # filter(character == "elephant") %>%
                    # mutate(age = scale(age)) %>% 
                    # column_to_rownames("age") %>%
                    select(-character, -age) %>%
                    as.matrix(),
                  p = 3,
                  rotation = "oblimin")
tfplot(temp)
