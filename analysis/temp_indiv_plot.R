library(tidyverse)
library(emo)

# BODY vs. HEART

temp2_BminH <- temp2 %>% 
  filter(comparison == "BminH", !is.na(age)) %>%
  mutate(sign = factor(sign(diff), labels = c("neg", "zero", "pos")))

contrasts(temp2_BminH$sign) <- cbind(neg_GM = c(1, -1, 0),
                                     pos_GM = c(0, -1, 1))

p1 <- glm(abs(diff) ~ scale(age, scale = F), # + scale(age, scale = F):sign,
          family = "poisson", data = temp2_BminH)

temp2_BminH$phat <- predict(p1, type = "response")

g1 <- ggplot(temp2_BminH %>%
               mutate(sign = factor(sign(diff),
                                    labels = c("HEART > BODY", 
                                               "no difference",
                                               "BODY > HEART"))), 
             aes(x = age, y = abs(diff))) +
  geom_point(position = position_jitter(width = 0, height = 0.2),
             size = 3,
             aes(color = character, shape = sign)) +
  geom_line(aes(y = phat)) +
  scale_x_continuous(breaks = 0:100) +
  scale_y_continuous(breaks = 0:100, limits = c(-0.2, 6.2)) +
  scale_color_brewer(palette = "Set3", guide = "none") +
  labs(title = "BODY vs. HEART",
       shape = "Direction of difference",
       color = "Target character",
       x = "Age (years)",
       y = "Difference in number of endorsements (0-6)") +
  theme_bw() +
  theme(text = element_text(size = 12),
        legend.position = "none")

# BODY vs. MIND

temp2_BminM <- temp2 %>% 
  filter(comparison == "BminM", !is.na(age)) %>%
  mutate(sign = factor(sign(diff), labels = c("neg", "zero", "pos")))

contrasts(temp2_BminM$sign) <- cbind(neg_GM = c(1, -1, 0),
                                     pos_GM = c(0, -1, 1))

p2 <- glm(abs(diff) ~ scale(age, scale = F), # + scale(age, scale = F):sign,
          family = "poisson", data = temp2_BminM)

temp2_BminM$phat <- predict(p2, type = "response")

g2 <- ggplot(temp2_BminM %>%
               mutate(sign = factor(sign(diff),
                                    labels = c("MIND > BODY", 
                                               "no difference",
                                               "BODY > MIND"))), 
             aes(x = age, y = abs(diff))) +
  geom_point(position = position_jitter(width = 0, height = 0.2),
             size = 3,
             aes(color = character, shape = sign)) +
  geom_line(aes(y = phat)) +
  scale_x_continuous(breaks = 0:100) +
  scale_y_continuous(breaks = 0:100, limits = c(-0.2, 6.2)) +
  scale_color_brewer(palette = "Set3", guide = "legend") +
  labs(title = "BODY vs. MIND",
       shape = "Direction of difference",
       color = "Target character",
       x = "Age (years)",
       y = "Difference in number of endorsements (0-6)") +
  theme_bw() +
  theme(text = element_text(size = 12), 
        legend.position = "none", legend.box = "vertical") +
  guides(shape = guide_legend(order = 1),
         color = guide_legend(order = 0))

# HEART vs. MIND

temp2_HminM <- temp2 %>% 
  filter(comparison == "HminM", !is.na(age)) %>%
  mutate(sign = factor(sign(diff), labels = c("neg", "zero", "pos")))

contrasts(temp2_HminM$sign) <- cbind(neg_GM = c(1, -1, 0),
                                     pos_GM = c(0, -1, 1))

p3 <- glm(abs(diff) ~ scale(age, scale = F), # + scale(age, scale = F):sign,
          family = "poisson", data = temp2_HminM)

temp2_HminM$phat <- predict(p3, type = "response")

g3 <- ggplot(temp2_HminM %>%
               mutate(sign = factor(sign(diff),
                                    labels = c("MIND > HEART", 
                                               "no difference",
                                               "HEART > MIND"))), 
             aes(x = age, y = abs(diff))) +
  geom_point(position = position_jitter(width = 0, height = 0.2),
             size = 3,
             aes(color = character, shape = sign)) +
  geom_line(aes(y = phat)) +
  scale_x_continuous(breaks = 0:100) +
  scale_y_continuous(breaks = 0:100, limits = c(-0.2, 6.2)) +
  scale_color_brewer(palette = "Set3", guide = "none") +
  labs(title = "HEART vs. MIND",
       shape = "Direction of difference",
       color = "Target character",
       x = "Age (years)",
       y = "Difference in number of endorsements (0-6)") +
  theme_bw() +
  theme(text = element_text(size = 12),
        legend.position = "none")

cowplot::plot_grid(g1, g2, g3, nrow = 1)




# -------


temp2_all <- temp2 %>%
  spread(comparison, diff) %>%
  mutate(BminH_sign = 
           factor(sign(BminH),
                  labels = c("HEART > BODY", "no difference", 
                             "BODY > HEART")),
         BminM_sign = 
           factor(sign(BminM),
                  labels = c("MIND > BODY", "no difference", 
                             "BODY > MIND")),
         HminM_sign = 
           factor(sign(HminM),
                  labels = c("MIND > HEART", "no difference", 
                             "HEART > MIND"))) %>%
  select(-BminH, -BminM, -HminM) %>%
  gather(comparison, sign, ends_with("_sign")) %>%
  mutate(comparison = gsub("_sign", "", comparison),
         comparison_lab = recode(comparison,
                                 "BminH" = "BODY vs. HEART",
                                 "BminM" = "BODY vs. MIND",
                                 "HminM" = "HEART vs. MIND")) %>%
  left_join(temp2) %>%
  distinct() %>%
  filter(!is.na(age)) %>%
  arrange(comparison)

temp2_all$phat <- c(predict(p1, type = "response"),
                    predict(p2, type = "response"),
                    predict(p3, type = "response"))

d_pval <- data.frame(label =
                       c(paste("b =", 
                               format(
                                 round(summary(p1)$coefficients[
                                   "scale(age, scale = F)", "Estimate"], 2),
                                 nsmall = 2),
                               "\np =",
                               format(
                                 round(summary(p1)$coefficients[
                                 "scale(age, scale = F)", "Pr(>|z|)"], 3),
                                 nsmall = 3)),
                         paste("b =", 
                               format(
                                 round(summary(p2)$coefficients[
                                   "scale(age, scale = F)", "Estimate"], 2),
                                 nsmall = 2),
                               "\np =",
                               format(
                                 round(summary(p2)$coefficients[
                                   "scale(age, scale = F)", "Pr(>|z|)"], 3),
                                 nsmall = 3)),
                         paste("b =", 
                               format(
                                 round(summary(p3)$coefficients[
                                   "scale(age, scale = F)", "Estimate"], 2),
                                 nsmall = 2),
                               "\np =",
                               format(
                                 round(summary(p3)$coefficients[
                                   "scale(age, scale = F)", "Pr(>|z|)"], 3),
                                 nsmall = 3))),
                     comparison_lab = c("BODY vs. HEART",
                                        "BODY vs. MIND",
                                        "HEART vs. MIND"))

ggplot(temp2_all,
       aes(x = age, y = abs(diff), color = character, shape = sign)) +
  facet_grid(~ comparison_lab) +
  geom_point(position = position_jitter(width = 0, height = 0.2),
             size = 5,
             aes(color = character, shape = sign)) +
  geom_line(aes(x = age, y = phat, group = comparison),
            color = "black") +
  geom_text(data = d_pval, 
            aes(x = min(temp2_all$age), y = 6, 
                color = NULL, shape = NULL, label = label),
            hjust = 0) +
  scale_x_continuous(breaks = 0:100) +
  scale_y_continuous(breaks = 0:100, limits = c(-0.2, 6.2)) +
  scale_color_brewer(palette = "Paired") +
  scale_shape_manual(values = c("\u25A0", "\u25A0", 
                                "\u2665", "\u2665", 
                                "\u25CF", "\u25CF",
                                "\u25D8")) +
  labs(title = "Differentiation of BODY, HEART, and MIND capacities by individual participants",
       shape = "Direction of difference",
       color = "Target character",
       x = "Age (years)",
       y = "Absolute difference in number of endorsements") +
  theme_bw() +
  theme(text = element_text(size = 16),
        legend.position = "right")

ggplot(temp2_all,
       aes(x = age, y = abs(diff))) +
  facet_grid(~ comparison_lab) +
  geom_point(aes(shape = character, # size = character,
                 color = sign, fill = sign),
             alpha = 0.8, size = 3,
             # stroke = 1,
             position = position_jitter(width = 0, height = 0.2)) +
  geom_line(aes(x = age, y = phat, group = comparison),
            color = "black") +
  # geom_smooth(aes(x = age, y = abs(diff), group = sign, color = sign),
  #             method = "lm") +
  geom_text(data = d_pval, 
            aes(x = min(temp2_all$age), y = 6, label = label),
            hjust = 0) +
  scale_x_continuous(breaks = 0:100) +
  scale_y_continuous(breaks = 0:100, limits = c(-0.2, 6.2)) +
  # scale_color_manual(values = c("#e41a1c", "#e41a1c", 
  #                               "#377eb8", "#377eb8", 
  #                               "#4daf4a", "#4daf4a",
  #                               "#999999")) +
  # scale_fill_manual(values = c("#fb9a99", "#fb9a99", 
  #                               "#a6cee3", "#a6cee3", 
  #                               "#b2df8a", "#b2df8a",
  #                               "#d9d9d9")) +
  scale_color_manual(values = c("#d95f02", "#d95f02", 
                                "#7570b3", "#7570b3", 
                                "#1b9e77", "#1b9e77",
                                "#999999")) +
  scale_fill_manual(values = c("#fc8d62", "#fc8d62", 
                               "#8da0cb", "#8da0cb", 
                               "#66c2a5", "#66c2a5",
                               "#d9d9d9")) +
  # scale_shape_manual(values = c(22, 23, 24, 25, 10, 7, 20, 17, 15)) +
  scale_shape_manual(values = c(15, 20)) +
  # scale_size_manual(values = c(3, 3, 2, 2, 4, 4, 3, 3.5, 4)) +
  labs(title = "Differentiation of BODY, HEART, and MIND capacities by individual participants",
       # shape = "Character",
       # size = "Character",
       color = "Direction of difference",
       fill = "Direction of difference",
       x = "Age (years)",
       y = "Absolute difference in number of endorsements") +
  theme_bw() +
  theme(text = element_text(size = 16),
        legend.position = "right")

ggplot(temp2_all %>%
         mutate(comparison_lab = factor(comparison_lab,
                                        labels = c("HEART vs. BODY",
                                                   "MIND vs. BODY",
                                                   "MIND vs. HEART"))),
       aes(x = age, y = -1*diff)) +
  facet_grid(~ comparison_lab) +
  geom_point(aes(shape = character, size = character,
    color = sign), #, fill = sign),
    alpha = 0.7,
    stroke = 1, 
    position = position_jitter(width = 0, height = 0.3)) +
  annotate("text", 
           x = min(temp2_all$age), y = 6, size = 5, hjust = 0,
           label = c("HEART without BODY", 
                     "MIND without BODY", 
                     "MIND without HEART"),
           color = c("#7570b3", "#1b9e77", "#1b9e77")) +
  annotate("text", 
           x = min(temp2_all$age), y = -6, size = 5, hjust = 0,
           label = c("BODY without HEART", 
                     "BODY without MIND", 
                     "HEART without MIND"),
           color = c("#d95f02", "#d95f02", "#7570b3")) +
  scale_x_continuous(breaks = 0:100) +
  scale_y_continuous(breaks = -100:100, limits = c(-6.3, 6.3)) +
  scale_color_manual(values = c("#d95f02", "#d95f02", 
                                "#7570b3", "#7570b3", 
                                "#1b9e77", "#1b9e77",
                                "#999999")) +
  # scale_fill_manual(values = c("#fc8d62", "#fc8d62", 
  #                              "#8da0cb", "#8da0cb", 
  #                              "#66c2a5", "#66c2a5",
  #                              "#d9d9d9")) +
  # scale_shape_manual(values = c(22, 23, 24, 25, 10, 7, 20, 17, 15)) +
  scale_shape_manual(values = c(15, 20)) +
  scale_size_manual(values = c(2.5, 2.5, 1.5, 1.5, 3.5, 3.5, 2.5, 3, 3.5)) +
  labs(title = "Differentiation of BODY, HEART, and MIND capacities by individual participants",
       shape = "Character",
       size = "Character",
       color = "Direction of difference",
       # fill = "Direction of difference",
       x = "Age (years)",
       y = "Difference in number of endorsements") +
  theme_bw() +
  theme(text = element_text(size = 16),
        legend.position = "right")

