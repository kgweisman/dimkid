# first run dimkid_cogsci_analysis.Rmd

# for 7-9yo -----

# make efa function
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

# do overall efa (7-9y)
efa_79 <- fa_fun_hb(d3_all)

# make list of mcs
mc_list <- names(d3_all)

# do all combinations of deleting one mc
del_random <- list(NULL)

for(i in 1:length(mc_list)) {
  d <- d3_all %>%
    rownames_to_column("subid") %>%
    gather(key, value, -subid) %>%
    filter(key != mc_list[i]) %>%
    spread(key, value) %>%
    remove_rownames() %>%
    column_to_rownames("subid")
  del_random[[i]] <- fa_fun_hb(d, n_var = 19) %>% fa.sort()
  rm(d)
}

# get all loadings first way

# del_random_loadings <- c(1:19)
# 
# for(i in 1:length(mc_list)) {
#   loadings <- del_random[[i]]$loadings[] %>%
#     data.frame() %>%
#     rownames_to_column("capacity") %>%
#     gather(factor, loading, -capacity) %>%
#     group_by(capacity) %>%
#     top_n(1, abs(loading)) %>%
#     ungroup()
#   names(loadings) <- paste(names(loadings), mc_list[i], sep = "_")
#   del_random_loadings <- cbind(del_random_loadings, loadings)
#   rm(loadings)
# }
# 
# del_random_loadings %>%
#   select(-starts_with("loading")) %>%
#   # select(starts_with("capacity"), starts_with("factor")) %>%
#   View()

# add congruence with overall factor solution

del_random_congruence <- NULL

for(i in 1:length(mc_list)) {
  orig <- efa_79$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    filter(capacity != mc_list[i]) %>%
    remove_rownames() %>%
    column_to_rownames("capacity") %>%
    rename(BODY = MR1, HEART = MR3, MIND = MR2) # by hand!
  
  del_random_temp <- del_random[[i]]$loadings[]
  
  cong_temp <- fa.congruence(del_random_temp, orig) %>%
    data.frame() %>%
    rownames_to_column("factor") %>%
    gather(orig_factor, congruence, -factor) %>%
    mutate(del_random = mc_list[i]) %>%
    select(del_random, factor, orig_factor, congruence)
  
  del_random_congruence <- rbind(del_random_congruence, cong_temp)
  
  rm(orig, del_random_temp, cong_temp)
}

# get all loadings second way

del_random_loadings2 <- data.frame()

for(i in 1:length(mc_list)) {
  loadings <- del_random[[i]]$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, -capacity) %>%
    mutate(del_random = mc_list[i]) %>%
    arrange(factor, abs(loading))
  del_random_loadings2 <- rbind(del_random_loadings2, loadings)
  rm(loadings)
}

del_random_loadings_cong <- del_random_congruence %>%
  group_by(del_random, factor) %>%
  top_n(1, congruence) %>%
  full_join(del_random_loadings2) %>%
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

# plot congruence
# del_random_congruence <- del_random_congruence %>%
#   full_join(del_random_loadings_cong %>% 
#               group_by(orig_factor, short) %>% 
#               summarise(median_loading = median(loading)) %>%
#               arrange(orig_factor, desc(abs(median_loading))) %>%
#               ungroup() %>%
#               group_by(short) %>%
#               top_n(1, median_loading) %>%
#               data.frame() %>%
#               rownames_to_column("order") %>%
#               mutate(order = as.numeric(as.character(order))))

ggplot(del_random_congruence,
       aes(x = orig_factor, 
           y = factor(factor, levels = c("MR2", "MR3", "MR1")), 
           fill = congruence, label = congruence)) +
  facet_wrap(~ del_random) +
  geom_tile() +
  geom_text() +
  scale_fill_distiller(palette = "RdYlBu", 
                       limits = c(0, 1), breaks = c(0, 1),
                       guide = guide_colorbar(title = element_blank(),
                                              barheight = 20)) +
  theme_bw()

# plot loadings across deletions

ggplot(del_random_loadings_cong,
       aes(x = loading, fill = orig_factor, color = orig_factor)) +
  facet_wrap(~ reorder(short, order), ncol = 5) +
  geom_vline(aes(xintercept = 0), lty = 2, size = 0.5) +
  # facet_grid(capacity ~ orig_factor) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 40) +
  # geom_density(alpha = 0.5) +
  theme_bw() +
  scale_fill_brewer("Factor (according to congruence with overall factors): ",
                    palette = "Set1") +
  scale_color_brewer("Factor (according to congruence with overall factors): ",
                    palette = "Set1") +
  theme(legend.position = "bottom")
