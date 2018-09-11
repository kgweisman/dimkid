ggplot(d4 %>% 
         filter(!is.na(response)) %>%
         mutate(response = factor(response, 
                                  levels = c("no", "kinda", "yes"))) %>%
         left_join(efa_d4_3$loadings[] %>%
                     data.frame() %>%
                     rownames_to_column("capacity") %>%
                     gather(factor, loading, -capacity) %>%
                     group_by(capacity) %>%
                     top_n(1, abs(loading)) %>% 
                     ungroup() %>%
                     arrange(factor, loading) %>%
                     mutate(order = 1:18) %>%
                     select(capacity, factor, order)) %>%
         mutate(factor_adult = case_when(
           capacity %in% bucket_adult4_BODY ~ "BODY",
           capacity %in% bucket_adult4_HEART ~ "HEART",
           capacity %in% bucket_adult4_MIND ~ "MIND")) %>%
         mutate(age_gp = case_when(
           age_years < median(d4$age_years) ~ "younger",
           age_years >= median(d4$age_years) ~ "older")) %>%
         mutate(age_gp = factor(age_gp, levels = c("younger", "older"))),
       aes(x = reorder(capacity, order), fill = response)) +
  facet_grid(factor_adult ~ character ~ age_gp, scales = "free", space = "free") +
  geom_bar(position = "fill") +
  geom_hline(yintercept = 0.5, lty = 2) +
  scale_fill_brewer(palette = "Blues") +
  coord_flip()
