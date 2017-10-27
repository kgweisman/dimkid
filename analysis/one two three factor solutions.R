# first run dimkid_cogsci_analysis.Rmd

# make 1-, 2-, 3-factor solutions -----

efa_1 <- fa(d34_all, nfactors = 1, rotate = "oblimin", scores = "tenBerge")
efa_1_loadings <- loadings(efa_1)[] %>% 
  data.frame() %>% 
  rownames_to_column("capacity") %>%
  distinct()
efa_1_scores <- efa_1$scores[] %>% 
  data.frame() %>% 
  rownames_to_column("subid") %>%
  mutate(subid = gsub(".*_", "", subid)) %>%
  full_join(d3 %>% select(subid, character, age_group, age) %>%
              full_join(d4 %>% select(subid, character, age_group, age))) %>%
  gather(factor, score, starts_with("MR")) %>% 
  distinct()

efa_2 <- fa(d34_all, nfactors = 2, rotate = "oblimin", scores = "tenBerge")
efa_2_loadings <- loadings(efa_2)[] %>% 
  data.frame() %>% 
  rownames_to_column("capacity") %>%
  distinct()
efa_2_scores <- efa_2$scores[] %>% 
  data.frame() %>% 
  rownames_to_column("subid") %>%
  mutate(subid = gsub(".*_", "", subid)) %>%
  full_join(d3 %>% select(subid, character, age_group, age) %>%
              full_join(d4 %>% select(subid, character, age_group, age))) %>%
  gather(factor, score, starts_with("MR")) %>%
  distinct()

efa_3 <- fa(d34_all, nfactors = 3, rotate = "oblimin", scores = "tenBerge")
efa_3_loadings <- loadings(efa_3)[] %>% 
  data.frame() %>% 
  rownames_to_column("capacity") %>%
  distinct()
efa_3_scores <- efa_3$scores[] %>% 
  data.frame() %>% 
  rownames_to_column("subid") %>%
  mutate(subid = gsub(".*_", "", subid)) %>%
  full_join(d3 %>% select(subid, character, age_group, age) %>%
              full_join(d4 %>% select(subid, character, age_group, age))) %>%
  gather(factor, score, starts_with("MR")) %>%
  distinct()

# plots (exact age) -----

ggplot(efa_1_scores %>%
         ungroup() %>%
         mutate(factor = factor(factor,
                                levels = c("MR1"),
                                labels = c("EVERYTHING")),
                character = factor(character,
                                   levels = c("computer", "robot", "doll", 
                                              "teddy_bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant")),
                age_group = factor(age_group,
                                   levels = c("children_46", "children_79"))),
       aes(x = age, y = score)) +
  facet_grid(factor ~ character) +
  # facet_grid(character ~ factor) +
  theme_bw() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  # geom_smooth(method = "loess", alpha = 0.4) +
  geom_smooth(method = "lm", alpha = 0.2, color = "black") +
  # geom_smooth(method = "lm", alpha = 0.4, formula = y ~ poly(x, 2)) +
  # geom_smooth(method = "lm", alpha = 0.4, formula = y ~ poly(x, 3)) +
  geom_point(size = 2, aes(color = age_group)) +
  scale_x_continuous(breaks = seq(2, 12, 2)) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(title = "One-factor solution",
    # subtitle = "Children (Studies 3-4)\n",
    x = "Age (years)",
    y = "Factor score") # 1200 by 300

ggplot(efa_2_scores %>%
         ungroup() %>%
         mutate(factor = factor(factor,
                                levels = c("MR1", "MR2"),
                                labels = c("HEART-BODY", "MIND")),
                character = factor(character,
                                   levels = c("computer", "robot", "doll", 
                                              "teddy_bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant")),
                age_group = factor(age_group,
                                   levels = c("children_46", "children_79"))),
       aes(x = age, y = score)) +
  facet_grid(factor ~ character) +
  # facet_grid(character ~ factor) +
  theme_bw() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  # geom_smooth(method = "loess", alpha = 0.4) +
  geom_smooth(method = "lm", alpha = 0.2, color = "black") +
  # geom_smooth(method = "lm", alpha = 0.4, formula = y ~ poly(x, 2)) +
  # geom_smooth(method = "lm", alpha = 0.4, formula = y ~ poly(x, 3)) +
  geom_point(size = 2, aes(color = age_group)) +
  scale_x_continuous(breaks = seq(2, 12, 2)) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(title = "Two-factor solution",
       # subtitle = "Children (Studies 3-4)\n",
       x = "Age (years)",
       y = "Factor score") # 1200 by 500

ggplot(efa_3_scores %>%
         ungroup() %>%
         mutate(factor = factor(factor,
                                levels = c("MR1", "MR2", "MR3"),
                                labels = c("HEART", "MIND", "BODY")),
                character = factor(character,
                                   levels = c("computer", "robot", "doll", 
                                              "teddy_bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant")),
                age_group = factor(age_group,
                                   levels = c("children_46", "children_79"))),
       aes(x = age, y = score)) +
  facet_grid(factor ~ character) +
  # facet_grid(character ~ factor) +
  theme_bw() +
  theme(text = element_text(size = 16),
        legend.position = "none") +
  # geom_smooth(method = "loess", alpha = 0.4) +
  geom_smooth(method = "lm", alpha = 0.2, color = "black") +
  # geom_smooth(method = "lm", alpha = 0.4, formula = y ~ poly(x, 2)) +
  # geom_smooth(method = "lm", alpha = 0.4, formula = y ~ poly(x, 3)) +
  geom_point(size = 2, aes(color = age_group)) +
  scale_x_continuous(breaks = seq(2, 12, 2)) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(title = "Three-factor solution",
       # subtitle = "Children (Studies 3-4)\n",
       x = "Age (years)",
       y = "Factor score") # 1200 by 700


# plots (age group) -----

ggplot(efa_1_scores %>%
         group_by(age_group, character, factor) %>%
         do(data.frame(rbind(smean.cl.boot(.$score)))) %>%
       ungroup() %>%
         mutate(character = factor(character, 
                                   levels = c("computer", "robot", "doll", 
                                              "teddy_bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant"),
                                   labels = c("computer", "robot", "doll", 
                                              "teddy bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant")),
                factor = factor(factor,
                                levels = c("MR1"),
                                labels = c("EVERYTHING")),
                age_group = factor(age_group,
                                   levels = c("children_46", "children_79"),
                                   labels = c("4-6y", "7-9y"))),
       aes(x = character, y = Mean, 
           color = age_group)) +
  facet_wrap(~ factor, ncol = 3) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "bottom") +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                width = 0.2, position = position_dodge(width = 0.4)) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(title = "One-factor solution",
    x = "Character",
    y = "Mean factor score",
    color = "Age group: ") # 400 by 500

ggplot(efa_2_scores %>%
         group_by(age_group, character, factor) %>%
         do(data.frame(rbind(smean.cl.boot(.$score)))) %>%
         ungroup() %>%
         mutate(character = factor(character, 
                                   levels = c("computer", "robot", "doll", 
                                              "teddy_bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant"),
                                   labels = c("computer", "robot", "doll", 
                                              "teddy bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant")),
                factor = factor(factor,
                                levels = c("MR1", "MR2"),
                                labels = c("HEART-BODY", "MIND")),
                age_group = factor(age_group,
                                   levels = c("children_46", "children_79"),
                                   labels = c("4-6y", "7-9y"))),
       aes(x = character, y = Mean, 
           color = age_group)) +
  facet_wrap(~ factor, ncol = 3) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "bottom") +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                width = 0.2, position = position_dodge(width = 0.4)) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(title = "Two-factor solution",
       x = "Character",
       y = "Mean factor score",
       color = "Age group: ") # 700 by 500

ggplot(efa_3_scores %>%
         group_by(age_group, character, factor) %>%
         do(data.frame(rbind(smean.cl.boot(.$score)))) %>%
         ungroup() %>%
         mutate(character = factor(character, 
                                   levels = c("computer", "robot", "doll", 
                                              "teddy_bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant"),
                                   labels = c("computer", "robot", "doll", 
                                              "teddy bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant")),
                factor = factor(factor,
                                levels = c("MR1", "MR2", "MR3"),
                                labels = c("HEART", "MIND", "BODY")),
                age_group = factor(age_group,
                                   levels = c("children_46", "children_79"),
                                   labels = c("4-6y", "7-9y"))),
       aes(x = character, y = Mean, 
           color = age_group)) +
  facet_wrap(~ factor, ncol = 3) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "bottom") +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                width = 0.2, position = position_dodge(width = 0.4)) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(title = "Three-factor solution",
       x = "Character",
       y = "Mean factor score",
       color = "Age group: ") # 1000 by 500
