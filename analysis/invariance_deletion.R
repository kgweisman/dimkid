# first run dimkid_cogsci_analysis.Rmd

# make functions -----

# make basic efa function
fa_fun_hb <- function(data, n_var = 20) {
  # do factor analysis: maximal, unrotated
  efa_max_unrot <- fa(r = data, nfactors = floor(n_var/3),
                      rotate = "none", cor = chosenCorType)
  
  # examine eigenvalues and variance explained
  efa_max_unrot_nfactors <- print(efa_max_unrot)$Vaccounted %>%
    t() %>%
    data.frame() %>%
    filter(SS.loadings > 1, Proportion.Explained > 0.05) %>%
    count() %>%
    as.numeric()
  
  # do factor analysis: limited, rotated
  efa_lim_rot <- fa(r = data, nfactors = efa_max_unrot_nfactors,
                    rotate = chosenRotType, cor = chosenCorType,
                    scores = "tenBerge")
  
  # check that each of these factors is the dominant factor for at least one mental capacity item
  efa_lim_rot_nfactors <- fa.sort(loadings(efa_lim_rot)[]) %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(loading_abs = abs(loading)) %>%
    group_by(capacity) %>%
    top_n(1, loading_abs) %>%
    ungroup() %>% 
    count(factor) %>% 
    filter(n > 0) %>% 
    nrow()
  
  # do factor analysis: final
  efa_final <- fa(r = data, nfactors = efa_lim_rot_nfactors,
                  rotate = chosenRotType, cor = chosenCorType,
                  alpha = 0.05) %>% # set alpha for RMSEA
    fa.sort()
  
  return(efa_final)
}

# make function to do all combinations of deleting one mc
del_random_fun <- function(df) {

  mc_list <- names(df)
  del_random <- list(NULL)
  
  for(i in 1:length(mc_list)) {
    d <- df %>%
      rownames_to_column("subid") %>%
      gather(key, value, -subid) %>%
      filter(key != mc_list[i]) %>%
      spread(key, value) %>%
      remove_rownames() %>%
      column_to_rownames("subid")
    del_random[[i]] <- fa_fun_hb(d, n_var = 19) %>% fa.sort()
    rm(d)
  }
  
  return(del_random)
}

# make function to get congruence with overall factor solution
del_random_congruence_fun <- function(group = c("7-9y", "4-6y")) {
  
  if(group == "7-9y") {
    df <- d3_all
  } else if(group == "4-6y") {
    df <- d4_all
  }
  
  del_random_df <- del_random_fun(df)
  mc_list <- names(df)
  efa_overall <- fa_fun_hb(df)

  del_random_congruence <- NULL
  
  for(i in 1:length(mc_list)) {
    orig <- efa_overall$loadings[] %>%
      data.frame() %>%
      rownames_to_column("capacity") %>%
      filter(capacity != mc_list[i]) %>%
      remove_rownames() %>%
      column_to_rownames("capacity")
    
    if(group == "7-9y") {
      orig <- orig %>% rename(BODY = MR1, HEART = MR3, MIND = MR2) # by hand!
    } else if(group == "4-6y") {
      orig <- orig %>% rename(BODY_HEART = MR1, MIND = MR2) # by hand!
    }
    
    del_random_temp <- del_random_df[[i]]$loadings[]
    
    cong_temp <- fa.congruence(del_random_temp, orig) %>%
      data.frame() %>%
      rownames_to_column("factor") %>%
      gather(orig_factor, congruence, -factor) %>%
      mutate(del_random = mc_list[i]) %>%
      select(del_random, factor, orig_factor, congruence)
    
    del_random_congruence <- rbind(del_random_congruence, cong_temp)
  }
  
  return(del_random_congruence)
}

# make function to get all loadings
del_random_loadings_fun <- function(df) {

  del_random_df <- del_random_fun(df)
  mc_list <- names(df)

  del_random_loadings <- NULL
  
  for(i in 1:length(mc_list)) {
    loadings <- del_random_df[[i]]$loadings[] %>%
      data.frame() %>%
      rownames_to_column("capacity") %>%
      gather(factor, loading, -capacity) %>%
      mutate(del_random = mc_list[i]) %>%
      arrange(factor, abs(loading))
    del_random_loadings <- rbind(del_random_loadings, loadings)
  }
  
  return(del_random_loadings)
}

# make function to tie together congruence and loadings
del_random_cong_load_fun <- function(group = c("7-9y", "4-6y")) {
  
  if(group == "7-9y") {
    df <- d3_all
  } else if(group == "4-6y") {
    df <- d4_all
  }
  
  congruence_temp <- del_random_congruence_fun(group)
  loadings_temp <- del_random_loadings_fun(df)
  
  del_random_loadings_cong <- congruence_temp %>%
    group_by(del_random, factor) %>%
    top_n(1, congruence) %>%
    full_join(loadings_temp) %>%
    rename(item = capacity) %>%
    full_join(wording_s3) %>%
    rename(capacity = item)
  
  del_random_loadings_cong <- del_random_loadings_cong %>%
    full_join(del_random_loadings_cong %>% 
                group_by(orig_factor, short) %>% 
                summarise(median_loading = median(loading)) %>%
                arrange(orig_factor, desc(abs(median_loading))) %>%
                ungroup() %>%
                group_by(short) %>%
                top_n(1, median_loading) %>%
                data.frame() %>%
                rownames_to_column("order") %>%
                mutate(order = as.numeric(as.character(order))) %>%
                select(short, order))
  
  return(del_random_loadings_cong)
}

# 7-9y -----

df_79_del <- del_random_cong_load_fun("7-9y")
df_79_cong <- del_random_congruence_fun("7-9y") %>%
  full_join(wording_s3, by = c("del_random" = "item"))

df_79 <- fa_fun_hb(d3_all)$loadings[] %>%
  data.frame() %>%
  rownames_to_column("capacity") %>%
  gather(orig_factor, orig_loading, -capacity) %>%
  full_join(wording_s3, by = c("capacity" = "item")) %>%
  distinct() %>%
  mutate(orig_factor = recode(orig_factor,
                              MR1 = "BODY",
                              MR3 = "HEART",
                              MR2 = "MIND"))

# plot congruence across deletions
ggplot(df_79_cong,
       aes(x = orig_factor, 
           y = factor(factor, levels = c("MR2", "MR3", "MR1")), 
           fill = congruence, label = congruence)) +
  facet_wrap(~ short) +
  geom_tile() +
  geom_text() +
  scale_fill_distiller(palette = "RdYlBu", 
                       limits = c(0, 1), breaks = c(0, 1),
                       guide = guide_colorbar(title = element_blank(),
                                              barheight = 20)) +
  theme_bw() +
  labs(title = "congruence across deletions: 7-9y",
       subtitle = "by deleted MC (facet labels)",
       x = "overall factor (full solution, 20 MCs)",
       y = "factor after deletion (19 MCs)")

# plot loadings across deletions
ggplot(df_79_del %>%
         full_join(df_79) %>%
         distinct(),
       aes(x = loading, fill = orig_factor, color = orig_factor)) +
  facet_wrap(~ reorder(short, order), ncol = 5) +
  geom_vline(aes(xintercept = 0), color = "black", 
             lty = 3, size = 0.5, alpha = 0.7) +
  geom_point(aes(x = orig_loading, y = -1, color = orig_factor)) +
  # facet_grid(capacity ~ orig_factor) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 40) +
  # geom_density(alpha = 0.5) +
  theme_bw() +
  scale_fill_brewer("factor (according to congruence with overall factors): ",
                    palette = "Set1") +
  scale_color_brewer("factor (according to congruence with overall factors): ",
                     palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(title = "factor loadings across deletions: 7-9y",
       subtitle = "by MC (facet labels); loadings from complete solution marked as points",
       x = "factor loading (-1 = perfect neg., +1 = perfect pos.)",
       y = "count of loadings across deletions")

# 4-6y -----

df_46_del <- del_random_cong_load_fun("4-6y")
df_46_cong <- del_random_congruence_fun("4-6y") %>%
  full_join(wording_s4, by = c("del_random" = "item"))

df_46 <- fa_fun_hb(d4_all)$loadings[] %>%
  data.frame() %>%
  rownames_to_column("capacity") %>%
  gather(orig_factor, orig_loading, -capacity) %>%
  full_join(wording_s4, by = c("capacity" = "item")) %>%
  distinct() %>%
  mutate(orig_factor = recode(orig_factor,
                              MR1 = "BODY_HEART",
                              MR2 = "MIND"))
  
# plot congruence across deletions
ggplot(df_46_cong,
       aes(x = orig_factor, 
           y = factor(factor, levels = c("MR2", "MR3", "MR1")), 
           fill = congruence, label = congruence)) +
  facet_wrap(~ short) +
  geom_tile() +
  geom_text() +
  scale_fill_distiller(palette = "RdYlBu", 
                       limits = c(0, 1), breaks = c(0, 1),
                       guide = guide_colorbar(title = element_blank(),
                                              barheight = 20)) +
  theme_bw() +
  labs(title = "congruence across deletions: 4-6y",
       subtitle = "by deleted MC (facet labels)",
       x = "overall factor (full solution, 20 MCs)",
       y = "factor after deletion (19 MCs)")

# plot loadings across deletions
ggplot(df_46_del %>%
         full_join(df_46) %>%
         distinct(),
       aes(x = loading, fill = orig_factor, color = orig_factor)) +
  facet_wrap(~ reorder(short, order), ncol = 5) +
  geom_vline(aes(xintercept = 0), color = "black", 
             lty = 3, size = 0.5, alpha = 0.7) +
  geom_point(aes(x = orig_loading, y = -1, color = orig_factor)) +
  # facet_grid(capacity ~ orig_factor) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 40) +
  # geom_density(alpha = 0.5) +
  theme_bw() +
  scale_fill_manual("factor (according to congruence with overall factors): ",
                    values = c("#984ea3", "#4daf4a")) +
  scale_color_manual("factor (according to congruence with overall factors): ",
                    values = c("#984ea3", "#4daf4a")) +
  theme(legend.position = "bottom") +
  labs(title = "factor loadings across deletions: 4-6y",
       subtitle = "by MC (facet labels); loadings from complete solution marked as points",
       x = "factor loading (-1 = perfect neg., +1 = perfect pos.)",
       y = "count of loadings across deletions")
