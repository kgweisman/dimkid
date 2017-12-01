# library(gganimate)

fa.parallel(d_slide_all %>% select(-character, -age))

VSS(d_slide_all %>% select(-character, -age), n = floor(20/3))

temp <- fa(d_slide_all %>% select(-character, -age), 
   # nfactors = floor(20/3), 
   # nfactors = 4,
   nfactors = 3,
   rotate = "oblimin",
   scores = "tenBerge") %>%
  fa.sort()

temp$scores[] %>%
  data.frame() %>%
  rownames_to_column("subid") %>%
  left_join(d_slide %>%
              data.frame() %>%
              select(subid, character, age)) %>%
  filter(!is.na(MR1), !is.na(MR2), !is.na(MR3), !is.na(age)) %>%
  gather(factor, score, starts_with("MR")) %>%
  mutate(factor = recode_factor(factor,
                                "MR1" = "HEART",
                                "MR2" = "MIND",
                                "MR3" = "BODY"),
         character = factor(character,
                            levels = c("computer", "robot",
                                       "doll", "teddy_bear",
                                       "beetle", "bird", 
                                       "mouse", "goat",
                                       "elephant"))) %>%
  ggplot(aes(x = age, y = score, color = character, fill = character)) +
  facet_grid(~factor) +
  geom_smooth(method = "lm", alpha = 0.2) +
  geom_point() +
  scale_color_brewer(type = "div", palette = 1) +
  scale_fill_brewer(type = "div", palette = 1) +
  theme_bw()




adults_20 <- d1_all %>%
  data.frame() %>%
  rownames_to_column("subid") %>%
  gather(capacity, response, -subid) %>%
  filter(capacity %in% names(d3_all)) %>%
  spread(capacity, response) %>%
  remove_rownames() %>%
  column_to_rownames("subid")

adults_20_efa <- fa(adults_20, nfactors = 3, 
                    rotate = "oblimin", scores = "tenBerge")

predict_kids <- predict.psych(object = adults_20_efa, 
                              data = d_slide_all %>% select(-age, -character),
                              old.data = adults_20)


predict_kids_df <- predict_kids  %>% 
  data.frame() %>%
  rownames_to_column("subid") %>%
  filter(!is.na(MR1), !is.na(MR2), !is.na(MR3)) %>%
  gather(factor, score, -subid) %>%
  left_join(d_slide_all_complete %>%
              data.frame() %>%
              rownames_to_column("subid") %>%
              select(subid, character, age)) %>%
  mutate(factor = recode_factor(factor,
                                "MR1" = "HEART",
                                "MR2" = "BODY",
                                "MR3" = "MIND"),
         character = factor(character,
                            levels = c("computer", "robot",
                                       "doll", "teddy_bear",
                                       "beetle", "bird", 
                                       "mouse", "goat",
                                       "elephant")))

g <- ggplot(predict_kids_df,
       aes(x = age, y = score, color = character, fill = character,
           frame = age)) +
  facet_grid(~factor) +
  geom_smooth(method = "lm", alpha = 0.2, cumulative = TRUE) +
  geom_point(cumulative = TRUE) +
  scale_color_brewer(type = "div", palette = 1) +
  scale_fill_brewer(type = "div", palette = 1) +
  theme_bw()

# gganimate(g)

plot_ly(data = predict_kids_df %>% spread(factor, score),
        x = ~HEART, y = ~BODY, z = ~MIND,
        color = ~character, colors = "BrBG",
        size = ~age, sizes = c(50, 200))


