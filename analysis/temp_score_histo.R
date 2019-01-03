score_histo_fun <- function(df, which_efa, kinda = 1){
  how_many_cap <- loadings_fun(which_efa) %>%
    group_by(capacity) %>%
    top_n(1, loading) %>%
    ungroup() %>%
    count(factor)
  how_many_cap <- min(how_many_cap$n)
  
  loadings <- loadings_fun(which_efa) %>%
    group_by(capacity) %>%
    top_n(1, loading) %>%
    ungroup() %>%
    group_by(factor) %>%
    top_n(how_many_cap, loading) %>%
    ungroup()
  
  bypart <- df %>%
    filter(capacity %in% loadings$capacity) %>%
    select(-factor) %>%
    left_join(loadings) %>%
    group_by(age_group, subid, character, factor) %>%
    mutate(response_num = case_when(response == "kinda" ~ kinda,
                                    TRUE ~ response_num),
           score = sum(response_num, na.rm = T)) %>%
    ungroup() %>%
    distinct(subid, age_group, character, factor, score)
  
  plot <- bypart %>%
    ggplot(aes(x = score)) +
    facet_grid(factor ~ age_group) +
    geom_histogram(bins = how_many_cap) +
    theme_bw()
  
  return(plot)
}

score_histo_fun(d1_all, efa_3_d1_ad)
score_histo_fun(d1_all, efa_3_d1_79)

score_histo_fun(d2_all, efa_3_d2_ad)
score_histo_fun(d2_all, efa_3_d2_79)
score_histo_fun(d2_all, efa_3_d2_46)
score_histo_fun(d2_all, efa_2_d2_46)
