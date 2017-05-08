# RELIABILITY

# Cronbach's alpha
keys.list <- list(SOUL = c("embarrassed", "guilt", "pride", "disrespected", 
                           "depressed", "happy", "love", "angry"),
                  BODY = c("hungry", "fear", "odors", "pain", "tired", "nauseated"),
                  MIND = c("choices", "temperature", "reasoning", "depth", 
                           "remembering", "conscious")) 
scores <- scoreItems(keys.list, d4, min = 0, max = 1)  # or just use the keys.lit
summary(scores)
scores
#The scores themselves are available in the scores$scores object.  I.e.,
describe(scores$scores)


# ICC
# use ICC2k (?)

# soul
ICC(d4[,c("embarrassed", "guilt", "pride", "disrespected", "depressed", "happy", "love", "angry")])

# body
ICC(d4[,c("hungry", "fear", "odors", "pain", "tired", "nauseated")])

# soul
ICC(d4[,c("choices", "temperature", "reasoning", "depth", "remembering", "conscious")])


# PLOT as sums
indiv_scores <- scores$scores %>%
  data.frame() %>%
  rownames_to_column(var = "subid") %>%
  gather(factor, score, -subid) %>%
  left_join(d2 %>% select(subid, character) %>% distinct())

indiv_scores_mb <- multi_boot(data = indiv_scores, 
                              column = "score", 
                              summary_groups = c("character", "factor"), 
                              statistics_functions = c("ci_lower", "mean", "ci_upper"))

ggplot(indiv_scores_mb %>%
         ungroup() %>%
         mutate(factor = factor(factor, levels = c("SOUL", "BODY", "MIND")),
                character = factor(character,
                                   levels = c("computer", "robot", 
                                              "doll", "teddy_bear", "beetle", 
                                              "bird", "mouse", "goat", "elephant"),
                                   labels = c("computer", "robot", 
                                              "doll", "teddy bear", "beetle", 
                                              "bird", "mouse", "goat", "elephant"))),
       aes(x = character, y = mean, color = character, shape = character)) +
  facet_wrap("factor", ncol = 3) +
  theme_bw() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none") +
  geom_point(size = 5, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, position = position_dodge(width = 0.4)) +
  # scale_color_manual(values = c("black", "#00BFC4", "#F8766D", rep("black", 4))) +
  # scale_shape_manual(values = c(17, 15, 19, rep(17, 4))) +
  scale_color_manual(values = c("black", "#00BFC4", rep("gray", 2), "#F8766D", rep("black", 4))) +
  scale_shape_manual(values = c(17, 15, rep(17, 2), 19, rep(17, 4))) +
  labs(title = "Subscale scores by character",
       subtitle = "(averages of items within each subscale)",
       x = "Character",
       y = "Mean subscale score") # 1000 by 500

indiv_scores %>% spread(factor, score) %>% select(BODY:SOUL) %>% cor.ci()
indiv_scores_mb %>% select(character, factor, mean) %>% spread(factor, mean) %>% ungroup() %>% select(BODY:SOUL) %>% cor.ci()
