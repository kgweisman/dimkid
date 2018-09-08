# study 3
s3_exclude <- d3 %>%
  distinct(subid, character, age) %>%
  mutate(subid2 = paste(character, subid, sep = "_")) %>%
  filter(is.na(age) | age < 7 | age >= 10)

temp_s3 <- d3_all %>%
  rownames_to_column("subid") %>%
  filter(!subid %in% s3_exclude$subid2) %>%
  column_to_rownames("subid")

fa(temp_s3, 13, rotate = "none",
   cor = chosenCorType, fm = "minres")

fa(temp_s3, 3, rotate = chosenRotType,
   cor = chosenCorType, fm = "minres")

fa.sort(loadings(fa(temp_s3, 3, rotate = chosenRotType, cor = chosenCorType, fm = "minres"))[]) %>%
  data.frame() %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, -capacity) %>%
  mutate(loading_abs = abs(loading)) %>%
  group_by(capacity) %>%
  top_n(1, loading_abs) %>%
  ungroup() %>%
  count(factor) %>% 
  filter(n > 0)

fa.sort(fa(temp_s3, 3, rotate = chosenRotType, cor = chosenCorType, fm = "minres"))

# study 4
s4_exclude <- d4 %>%
  distinct(subid, character, age) %>%
  mutate(subid2 = paste(character, subid, sep = "_")) %>%
  filter(is.na(age) | age < 4 | age >= 7)

temp_s4 <- d4_all %>%
  rownames_to_column("subid") %>%
  filter(!subid %in% s4_exclude$subid2) %>%
  column_to_rownames("subid")

fa(temp_s4, 13, rotate = "none",
   cor = chosenCorType, fm = "minres")

fa(temp_s4, 3, rotate = chosenRotType,
   cor = chosenCorType, fm = "minres")

fa.sort(loadings(fa(temp_s4, 3, rotate = chosenRotType, cor = chosenCorType, fm = "minres"))[]) %>%
  data.frame() %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, -capacity) %>%
  mutate(loading_abs = abs(loading)) %>%
  group_by(capacity) %>%
  top_n(1, loading_abs) %>%
  ungroup() %>%
  count(factor) %>% 
  filter(n > 0)

fa.sort(fa(temp_s4, 3, rotate = chosenRotType, cor = chosenCorType, fm = "minres"))

# combined
temp_s34 <- full_join(temp_s3 %>% rownames_to_column("subid"), 
                      temp_s4 %>% rownames_to_column("subid")) %>%
  column_to_rownames("subid")

fa(temp_s34, 13, rotate = "none",
   cor = chosenCorType, fm = "minres")

fa(temp_s34, 3, rotate = chosenRotType,
   cor = chosenCorType, fm = "minres")

fa.sort(loadings(fa(temp_s34, 3, rotate = chosenRotType, cor = chosenCorType, fm = "minres"))[]) %>%
  data.frame() %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, -capacity) %>%
  mutate(loading_abs = abs(loading)) %>%
  group_by(capacity) %>%
  top_n(1, loading_abs) %>%
  ungroup() %>%
  count(factor) %>% 
  filter(n > 0)

temp_efa <- fa(temp_s34, 3, rotate = chosenRotType, cor = chosenCorType, fm = "minres")
fa.sort(temp_efa)

temp_plotting <- d3 %>%
  select(age_group, subid, age, character) %>%
  distinct() %>%
  mutate(subid = paste(character, subid, sep = "_")) %>%
  full_join(d4 %>%
              select(age_group, subid, age, character) %>%
              distinct() %>%
              mutate(subid = paste(character, subid, sep = "_"))) %>%
  full_join(temp_efa$scores %>%
              data.frame() %>%
              rownames_to_column("subid")) %>%
  mutate(character = factor(character)) %>%
  # mutate(age = ifelse(age < 3.5, NA, age)) %>%
  rename(score_F1 = MR1, score_F2 = MR2, score_F3 = MR3) %>%
  filter(!is.na(score_F1), !is.na(score_F2), !is.na(score_F3), !is.na(age)) %>%
  gather(factor, score, starts_with("score_")) %>%
  mutate(factor = factor(factor))

ggplot(temp_plotting %>%
         ungroup() %>%
         mutate(factor = factor(factor,
                                levels = c("score_F1", "score_F2", "score_F3"),
                                labels = c("Social-emotional",
                                           "Perceptual-cognitive",
                                           "Bodily")),
                character = factor(character,
                                   levels = c("computer", "robot", "doll", "teddy_bear",
                                              "beetle", "bird", "mouse", "goat", "elephant"))),
       aes(x = age, y = score)) +
  # aes(x = age, y = score, group = age_group)) +
  # facet_wrap("factor", ncol = 3) +
  # facet_grid(factor ~ character) +
  facet_grid(character ~ factor) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position = "none") +
  # geom_smooth(method = "loess", alpha = 0.4) +
  geom_smooth(method = "lm", alpha = 0.4) +
  # geom_smooth(method = "lm", alpha = 0.4, formula = y ~ poly(x, 2)) +
  # geom_smooth(method = "lm", alpha = 0.4, formula = y ~ poly(x, 3)) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(2, 12, 2)) +
  # scale_fill_manual(values = c("black", "#00BFC4", rep("gray", 2), "#F8766D", rep("black", 4))) +
  # scale_color_manual(values = c("black", "#00BFC4", rep("gray", 2), "#F8766D", rep("black", 4))) +
  # scale_shape_manual(values = c(17, 15, rep(17, 2), 19, rep(17, 4))) +
  labs(title = "Factor scores by children's age",
       # subtitle = "Children (Studies 3-4)\n",
       x = "Age (years)",
       y = "Factor score") # 1000 by 500

temp_plotting <- temp_plotting %>%
  mutate(factor = factor(factor, labels = c("HEART", "MIND", "BODY")))
contrasts(temp_plotting$character) <- cbind(ani_inan = c(4, 4, -5, -5, 4, 4, 4, -5, -5),
                                            mamm_non = c(-3, -3, 0, 0, 2, 2, 2, 0, 0),
                                            brd_beet = c(-1, 1, 0, 0, 0, 0, 0, 0, 0),
                                            elgo_mou = c(0, 0, 0, 0, 1, 1, -2, 0, 0),
                                            ele_goat = c(0, 0, 0, 0, 1, -1, 0, 0, 0),
                                            tech_toy = c(0, 0, 1, -1, 0, 0, 0, 1, -1),
                                            rob_comp = c(0, 0, -1, 0, 0, 0, 0, 1, 0),
                                            ted_doll = c(0, 0, 0, -1, 0, 0, 0, 0, 1))
contrasts(temp_plotting$factor) <- cbind(HEART = c(1, -1, 0),
                                         BODY = c(0, -1, 1))

temp_plotting2 <- temp_plotting %>%
  mutate(ani_inan = ifelse(character %in% c("beetle", "bird", "elephant", "goat", "mouse"), T, F),
         mamm_non = ifelse(ani_inan,
                           ifelse(character %in% c("elephant", "goat", "mouse"), T, F),
                           NA),
         brd_beet = ifelse(mamm_non == F,
                           ifelse(character == "bird", T, F),
                           NA),
         elgo_mou = ifelse(mamm_non,
                           ifelse(character %in% c("elephant", "goat"), T, F),
                           NA),
         ele_goat = ifelse(elgo_mou,
                           ifelse(character == "elephant", T, F),
                           NA),
         tech_toy = ifelse(ani_inan == F,
                           ifelse(character %in% c("computer", "robot"), T, F),
                           NA),
         rob_comp = ifelse(tech_toy,
                           ifelse(character == "robot", T, F),
                           NA),
         ted_doll = ifelse(tech_toy == F,
                           ifelse(character == "teddy_bear", T, F),
                           NA),
         age_medsplit = factor(ifelse(age < median(age, na.rm = T), "young", "old"), levels = c("young", "old")))

r_temp <- lmer(score ~ scale(age, scale = F) * factor * character + (1 | subid), temp_plotting)
summary(r_temp)

# main effects
ggplot(temp_plotting2 %>% filter(!is.na(age)), 
       aes(x = age, y = score)) + geom_point() + geom_smooth(method = "lm") +
  labs(title = "main effect: scale(age, scale = F)",
       subtitle = "age in years, centered (excluding NAs)") +
  annotate("label", x = 4, y = 2, label = paste0("t = ", round(summary(r_temp)$coefficients["scale(age, scale = F)","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(factor)), aes(x = score, fill = factor)) + 
  geom_density(alpha = 0.5) +
  labs(title = "main effects: factorHEART, factorBODY",
       subtitle = "effect coded with MIND as base") +
  annotate("label", x = -2, y = 0.8, label = paste0("factorHEART: t = ", round(summary(r_temp)$coefficients["factorHEART","t value"], 2)), hjust = 0) +
  annotate("label", x = -2, y = 0.7, label = paste0("factorBODY: t = ", round(summary(r_temp)$coefficients["factorBODY","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(ani_inan)), aes(x = score, fill = ani_inan)) + 
  geom_density(alpha = 0.5) +
  labs(title = "main effect: characterani_inan",
       subtitle = "FALSE (inanimate): computer, doll, robot, teddy bear\nTRUE (animate): beetle, bird, elephant, goat, mouse") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterani_inan","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(mamm_non)), aes(x = score, fill = mamm_non)) + 
  geom_density(alpha = 0.5) +
  labs(title = "main effect: charactermamm_non",
       subtitle = "FALSE (non-mammal): bird, beetle\nTRUE (mammal): elephant, goat, mouse") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["charactermamm_non","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(brd_beet)), aes(x = score, fill = brd_beet)) + 
  geom_density(alpha = 0.5) +
  labs(title = "main effect: characterbrd_beet",
       subtitle = "FALSE: beetle\nTRUE: bird") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterbrd_beet","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(elgo_mou)), aes(x = score, fill = elgo_mou)) + 
  geom_density(alpha = 0.5) +
  labs(title = "main effect: characterelgo_mou",
       subtitle = "FALSE: mouse\nTRUE: elephant, goat") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterelgo_mou","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(ele_goat)), aes(x = score, fill = ele_goat)) + 
  geom_density(alpha = 0.5) +
  labs(title = "main effect: characterele_goat",
       subtitle = "FALSE: goat\nTRUE: elephant") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterele_goat","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(tech_toy)), aes(x = score, fill = tech_toy)) + 
  geom_density(alpha = 0.5) +
  labs(title = "main effect: charactertech_toy",
       subtitle = "FALSE (toy): doll, teddy bear\nTRUE (tech): computer, robot") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["charactertech_toy","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(rob_comp)), aes(x = score, fill = rob_comp)) + 
  geom_density(alpha = 0.5) +
  labs(title = "main effect: characterrob_comp",
       subtitle = "FALSE: computer\nTRUE: robot") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterrob_comp","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(ted_doll)), aes(x = score, fill = ted_doll)) + 
  geom_density(alpha = 0.5) +
  labs(title = "main effect: characterted_doll",
       subtitle = "FALSE: doll\nTRUE: teddy bear") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterted_doll","t value"], 2)), hjust = 0)

# 2-way interactions
# ... with age
ggplot(temp_plotting2 %>% filter(!is.na(factor)), aes(x = score, fill = factor)) + 
  facet_grid(~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "2-way interactions: scale(age, scale = F):factorHEART, scale(age, scale = F):factorBODY",
       subtitle = "effect coded with MIND as base") +
  annotate("label", x = -2, y = 0.8, label = paste0("scale(age, scale = F):factorHEART: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorHEART","t value"], 2)), hjust = 0) +
  annotate("label", x = -2, y = 0.7, label = paste0("scale(age, scale = F):factorBODY: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorBODY","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(ani_inan)), aes(x = score, fill = ani_inan)) + 
  facet_grid(~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "2-way interaction: scale(age, scale = F):characterani_inan",
       subtitle = "FALSE (inanimate): computer, doll, robot, teddy bear\nTRUE (animate): beetle, bird, elephant, goat, mouse") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):characterani_inan","t value"], 2)), hjust = 0)

ggplot(temp_plotting2 %>% filter(!is.na(mamm_non)), aes(x = score, fill = mamm_non)) + 
  facet_grid(~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "main effect: scale(age, scale = F):charactermamm_non",
       subtitle = "FALSE (non-mammal): bird, beetle\nTRUE (mammal): elephant, goat, mouse") +
  annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):charactermamm_non","t value"], 2)), hjust = 0)

# ggplot(temp_plotting2 %>% filter(!is.na(brd_beet)), aes(x = score, fill = brd_beet)) + 
#   geom_density(alpha = 0.5) +
#   labs(title = "main effect: characterbrd_beet",
#        subtitle = "FALSE: beetle\nTRUE: bird") +
#   annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterbrd_beet","t value"], 2)), hjust = 0)
# 
# ggplot(temp_plotting2 %>% filter(!is.na(elgo_mou)), aes(x = score, fill = elgo_mou)) + 
#   geom_density(alpha = 0.5) +
#   labs(title = "main effect: characterelgo_mou",
#        subtitle = "FALSE: mouse\nTRUE: elephant, goat") +
#   annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterelgo_mou","t value"], 2)), hjust = 0)
# 
# ggplot(temp_plotting2 %>% filter(!is.na(ele_goat)), aes(x = score, fill = ele_goat)) + 
#   geom_density(alpha = 0.5) +
#   labs(title = "main effect: characterele_goat",
#        subtitle = "FALSE: goat\nTRUE: elephant") +
#   annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterele_goat","t value"], 2)), hjust = 0)
# 
# ggplot(temp_plotting2 %>% filter(!is.na(tech_toy)), aes(x = score, fill = tech_toy)) + 
#   geom_density(alpha = 0.5) +
#   labs(title = "main effect: charactertech_toy",
#        subtitle = "FALSE (toy): doll, teddy bear\nTRUE (tech): computer, robot") +
#   annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["charactertech_toy","t value"], 2)), hjust = 0)
# 
# ggplot(temp_plotting2 %>% filter(!is.na(rob_comp)), aes(x = score, fill = rob_comp)) + 
#   geom_density(alpha = 0.5) +
#   labs(title = "main effect: characterrob_comp",
#        subtitle = "FALSE: computer\nTRUE: robot") +
#   annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterrob_comp","t value"], 2)), hjust = 0)
# 
# ggplot(temp_plotting2 %>% filter(!is.na(ted_doll)), aes(x = score, fill = ted_doll)) + 
#   geom_density(alpha = 0.5) +
#   labs(title = "main effect: characterted_doll",
#        subtitle = "FALSE: doll\nTRUE: teddy bear") +
#   annotate("label", x = -2, y = 0.7, label = paste0("t = ", round(summary(r_temp)$coefficients["characterted_doll","t value"], 2)), hjust = 0)

# 3-way interactions
ggplot(temp_plotting2 %>% filter(!is.na(ani_inan)), aes(x = score, fill = ani_inan)) + 
  facet_grid(factor ~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "3-way interactions: age * factor * (animate vs. inanimate)",
       subtitle = paste0("scale(age, scale = F):factorHEART:characterani_inan: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorHEART:characterani_inan","t value"], 2), "\nscale(age, scale = F):factorBODY:characterani_inan: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorBODY:characterani_inan","t value"], 2)))

ggplot(temp_plotting2 %>% filter(!is.na(mamm_non)), aes(x = score, fill = mamm_non)) + 
  facet_grid(factor ~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "3-way interactions: age * factor * (mammal vs. non-mammal animate)",
       subtitle = paste0("scale(age, scale = F):factorHEART:charactermamm_non: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorHEART:charactermamm_non","t value"], 2), "\nscale(age, scale = F):factorBODY:charactermamm_non: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorBODY:charactermamm_non","t value"], 2)))

ggplot(temp_plotting2 %>% filter(!is.na(brd_beet)), aes(x = score, fill = brd_beet)) + 
  facet_grid(factor ~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "3-way interactions: age * factor * (bird vs. beetle)",
       subtitle = paste0("scale(age, scale = F):factorHEART:characterbrd_beet: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorHEART:characterbrd_beet","t value"], 2), "\nscale(age, scale = F):factorBODY:characterbrd_beet: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorBODY:characterbrd_beet","t value"], 2)))

ggplot(temp_plotting2 %>% filter(!is.na(elgo_mou)), aes(x = score, fill = elgo_mou)) + 
  facet_grid(factor ~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "3-way interactions: age * factor * (elephant, goat vs. mouse)",
       subtitle = paste0("scale(age, scale = F):factorHEART:characterelgo_mou: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorHEART:characterelgo_mou","t value"], 2), "\nscale(age, scale = F):factorBODY:characterelgo_mou: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorBODY:characterelgo_mou","t value"], 2)))

ggplot(temp_plotting2 %>% filter(!is.na(ele_goat)), aes(x = score, fill = ele_goat)) + 
  facet_grid(factor ~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "3-way interactions: age * factor * (elephant vs. goat)",
       subtitle = paste0("scale(age, scale = F):factorHEART:characterele_goat: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorHEART:characterele_goat","t value"], 2), "\nscale(age, scale = F):factorBODY:characterele_goat: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorBODY:characterele_goat","t value"], 2)))

ggplot(temp_plotting2 %>% filter(!is.na(tech_toy)), aes(x = score, fill = tech_toy)) + 
  facet_grid(factor ~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "3-way interactions: age * factor * (tech vs. toy inanimate)",
       subtitle = paste0("scale(age, scale = F):factorHEART:charactertech_toy: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorHEART:charactertech_toy","t value"], 2), "\nscale(age, scale = F):factorBODY:charactertech_toy: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorBODY:charactertech_toy","t value"], 2)))

ggplot(temp_plotting2 %>% filter(!is.na(rob_comp)), aes(x = score, fill = rob_comp)) + 
  facet_grid(factor ~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "3-way interactions: age * factor * (robot vs. computer)",
       subtitle = paste0("scale(age, scale = F):factorHEART:characterrob_comp: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorHEART:characterrob_comp","t value"], 2), "\nscale(age, scale = F):factorBODY:characterrob_comp: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorBODY:characterrob_comp","t value"], 2)))

ggplot(temp_plotting2 %>% filter(!is.na(ted_doll)), aes(x = score, fill = ted_doll)) + 
  facet_grid(factor ~ age_medsplit) +
  geom_density(alpha = 0.5) +
  labs(title = "3-way interactions: age * factor * (teddy bear vs. doll)",
       subtitle = paste0("scale(age, scale = F):factorHEART:characterted_doll: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorHEART:characterted_doll","t value"], 2), "\nscale(age, scale = F):factorBODY:characterted_doll: t = ", round(summary(r_temp)$coefficients["scale(age, scale = F):factorBODY:characterted_doll","t value"], 2)))
