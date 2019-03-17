# first run ch04_organization.Rmd

# study 1a -----
temp_d1a <- efa_wdm_d1a_ad$scores[] %>%
  data.frame() %>%
  rownames_to_column("subid") %>%
  gather(factor, score, -subid) %>%
  mutate(factor = factor(factor, levels = paste0("F", 1:3),
                         labels = factor_names_efa_wdm_d1a_ad),
         character = gsub("R_.*_", "", subid))

BH_d1a <- ggplot(temp_d1a %>% spread(factor, score),
                 aes(x = BODY, y = HEART, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors02) +
  theme(legend.position = "bottom")

BM_d1a <- ggplot(temp_d1a %>% spread(factor, score),
                 aes(x = BODY, y = MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors02) +
  theme(legend.position = "bottom")

HM_d1a <- ggplot(temp_d1a %>% spread(factor, score),
                 aes(x = HEART, y = MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors02) +
  theme(legend.position = "bottom")

plot_grid(plot_grid(BH_d1a + theme(legend.position = "none"), 
                    BM_d1a + theme(legend.position = "none"), 
                    HM_d1a + theme(legend.position = "none"), 
                    ncol = 3),
          get_legend(BH_d1a), ncol = 1, rel_heights = c(1, 0.1))


# study 1b -----
temp_d1b <- efa_wdm_d1b_ad$scores[] %>%
  data.frame() %>%
  rownames_to_column("subid") %>%
  gather(factor, score, -subid) %>%
  mutate(factor = factor(factor, levels = paste0("F", 1:3),
                         labels = factor_names_efa_wdm_d1b_ad),
         character = gsub("R_.*_", "", subid))

BH_d1b <- ggplot(temp_d1b %>% spread(factor, score),
                 aes(x = BODY, y = HEART, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors02) +
  theme(legend.position = "bottom")

BM_d1b <- ggplot(temp_d1b %>% spread(factor, score),
                 aes(x = BODY, y = MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors02) +
  theme(legend.position = "bottom")

HM_d1b <- ggplot(temp_d1b %>% spread(factor, score),
                 aes(x = HEART, y = MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors02) +
  theme(legend.position = "bottom")

plot_grid(plot_grid(BH_d1b + theme(legend.position = "none"), 
                    BM_d1b + theme(legend.position = "none"), 
                    HM_d1b + theme(legend.position = "none"), 
                    ncol = 3),
          get_legend(BH_d1b), ncol = 1, rel_heights = c(1, 0.1))


# study 1c -----
temp_d1c <- efa_wdm_d1c_ad$scores[] %>%
  data.frame() %>%
  rownames_to_column("subid") %>%
  gather(factor, score, -subid) %>%
  mutate(factor = factor(factor, levels = paste0("F", 1:3),
                         labels = factor_names_efa_wdm_d1c_ad),
         character = gsub("R_.*_", "", subid))

BH_d1c <- ggplot(temp_d1c %>% spread(factor, score),
                 aes(x = BODY, y = HEART, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors02) +
  theme(legend.position = "bottom")

BM_d1c <- ggplot(temp_d1c %>% spread(factor, score),
                 aes(x = BODY, y = MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors02) +
  theme(legend.position = "bottom")

HM_d1c <- ggplot(temp_d1c %>% spread(factor, score),
                 aes(x = HEART, y = MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors02) +
  theme(legend.position = "bottom")

plot_grid(plot_grid(BH_d1c + theme(legend.position = "none"), 
                    BM_d1c + theme(legend.position = "none"), 
                    HM_d1c + theme(legend.position = "none"), 
                    ncol = 3),
          get_legend(BH_d1c), ncol = 1, rel_heights = c(1, 0.1))


# study 1d -----
temp_d1d <- efa_wdm_d1d_ad$scores[] %>%
  data.frame() %>%
  rownames_to_column("subid") %>%
  gather(factor, score, -subid) %>%
  mutate(factor = factor(factor, levels = paste0("F", 1:3),
                         labels = factor_names_efa_wdm_d1d_ad),
         character = gsub("R_.*_", "", subid))

BH_d1d <- ggplot(temp_d1d %>% spread(factor, score),
                 aes(x = BODY, y = HEART, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors21) +
  theme(legend.position = "bottom")

BM_d1d <- ggplot(temp_d1d %>% spread(factor, score),
                 aes(x = BODY, y = MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors21) +
  theme(legend.position = "bottom")

HM_d1d <- ggplot(temp_d1d %>% spread(factor, score),
                 aes(x = HEART, y = MIND, color = character)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = colors21) +
  theme(legend.position = "bottom")

plot_grid(plot_grid(BH_d1d + theme(legend.position = "none"), 
                    BM_d1d + theme(legend.position = "none"), 
                    HM_d1d + theme(legend.position = "none"), 
                    ncol = 3),
          get_legend(BH_d1d), ncol = 1, rel_heights = c(1, 0.1))



# could do this for all samples in all studies