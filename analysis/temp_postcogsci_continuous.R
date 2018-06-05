# ggplot(df_endorsements_diff,
#        aes(x = age, y = diff, color = comparison, fill = comparison)) +
#   geom_smooth(method = "lm", alpha = 0.1) +
#   geom_point() +
#   theme_minimal()

ggplot(df_endorsements_diff,
       aes(x = age, y = abs(diff), 
           group = comparison, 
           color = factor(sign(diff), 
                          labels = c("negative", "no difference", "positive")))) +
  facet_grid(~ comparison) +
  # geom_point(position = position_jitter(width = 0, height = 0.25), alpha = 0) +
  geom_smooth(method = "loess", color = "black", alpha = 0.2, span = 2) +
  scale_color_manual(values = c("#377eb8", "#999999", "#e41a1c")) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(x = "Age (years)", y = "Absolute difference", color = "Direction of difference")

contrasts(df_endorsements_diff$comparison) <- cbind(BH = c(1, -1, 0),
                                                    MH = c(0, -1, 1))
# r1 <- lme4::glmer(abs(diff) ~ scale(age, scale = F) * comparison +
#                     (1|subid) + (comparison|character), df_endorsements_diff,
#                   family = "poisson")
# summary(r1)


temp <- df_endorsements_diff %>%
  mutate(abs_diff = abs(diff),
         comparison = factor(comparison))

contrasts(temp$comparison) <- cbind(BH = c(1, -1, 0),
                                    MH = c(0, -1, 1))

r1 <- brms::brm(abs_diff ~ scale(age) * comparison +
                  (1|subid) + (comparison|character), 
                data = temp,
                family = poisson())
summary(r1)
