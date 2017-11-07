# first run dimkid_cogsci_analysis.Rmd
library(tidyverse)
library(psych)
library(langcog) # source: https://github.com/langcog/langcog
library(RColorBrewer)
library(plotly)
library(lubridate)
library(rms)
library(cowplot)

# make basic functions
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
fa_fun_pa <- function(data, n_var = 20) {
  # do parallel analysis
  nfact <- fa.parallel(data, fm = "minres", plot = FALSE)$nfact
  
  # do factor analysis: final
  efa_final <- fa(r = data, nfactors = nfact,
                  rotate = chosenRotType, cor = chosenCorType,
                  alpha = 0.05) %>% # set alpha for RMSEA
    fa.sort()
  
  return(efa_final)
}
fa_fun_BIC <- function(data, n_var = 20) {
  # do VSS
  VSS <- VSS(data, rotate = "none", n = floor(n_var/3),
             fm = "minres", plot = FALSE)$vss.stats %>%
    rownames_to_column("nfact") %>% 
    mutate(nfact = as.numeric(as.character(nfact))) %>%
    top_n(-1, BIC)
  nfact <- VSS$nfact
  
  # do factor analysis: final
  efa_final <- fa(r = data, nfactors = nfact,
                  rotate = chosenRotType, cor = chosenCorType,
                  alpha = 0.05) %>% # set alpha for RMSEA
    fa.sort()
  
  return(efa_final)
}

# choose which efa function
# fa_fun <- fa_fun_hb
# fa_fun <- fa_fun_pa
fa_fun <- fa_fun_BIC

# make function to replace N% of responses
replace_nprop_fun <- function(df, prop_rep) {
  df_new <- df %>%
    rownames_to_column("subid") %>%
    gather(capacity, response, -subid) %>%
    group_by(capacity, response, subid) %>%
    mutate(rand = sample(x = c(0, 1), size = 1, 
                         prob = c(1 - prop_rep, prop_rep))) %>%
    ungroup() %>%
    group_by(capacity, subid, response, rand) %>%
    mutate(response_jit = ifelse(rand == 0, response,
                                 sample(x = c(0, 0.5, 1),
                                        size = 1,
                                        prob = c(1/3, 1/3, 1/3)))) %>%
    ungroup() %>%
    select(subid, capacity, response_jit) %>%
    spread(capacity, response_jit) %>%
    remove_rownames() %>%
    data.frame() %>%
    column_to_rownames("subid")
    
  return(df_new)
}

d3_orig_efa <- fa_fun(d3_all, n_var = 20)$loadings[] %>%
  data.frame() %>%
  rename(BODY = MR1, HEART = MR3, MIND = MR2)

d3_young_efa <- fa_fun(d4_all, n_var = 20)$loadings[] %>%
  data.frame() %>%
  rename(HEART_BODY = MR1, MIND = MR2)

young_cong <- fa.congruence(d3_young_efa, d3_orig_efa) %>%
  data.frame() %>% 
  rownames_to_column("factor") %>%
  gather(orig_factor, congruence, -factor)

# make function to get congruence data
congruence_fun <- function(df, orig_efa, nvar = 20, prop_rep, niter) {
  
  # get congruence over n iterations
  congruence1 <- NULL

  for(i in 1:niter) {
    new_df <- replace_nprop_fun(df, prop_rep)
    efa <- fa_fun(new_df, n_var = nvar)
    cong <- fa.congruence(efa, orig_efa) %>%
      data.frame() %>%
      rownames_to_column("factor") %>%
      gather(orig_factor, congruence, -factor) %>%
      mutate(iter = i)
    
    congruence1 <- rbind(congruence1, cong)
  }
  
  # get top congruence by factor for each iter
  congruence2 <- congruence1 %>%
    group_by(factor, iter) %>%
    top_n(1, congruence)
  
  congruence3 <- congruence2 %>%
    group_by(orig_factor) %>%
    do(data.frame(rbind(smean.cl.boot(.$congruence))))
  
  congruence4 <- congruence2 %>% 
    ungroup() %>% 
    group_by(orig_factor) %>% 
    count() %>%
    data.frame()
  
  cong_list <- list(congruence1, congruence2, congruence3, congruence4)
  
  return(cong_list)
  
}

# make function to plot congruence
plot_fun <- function(cong_list, prop_rep) {
  
  congruence2 <- cong_list[[2]]
  congruence3 <- cong_list[[3]]
  congruence4 <- cong_list[[4]]
  congruence5 <- cong_list[[2]] %>%
    ungroup() %>%
    count(iter, orig_factor) %>%
    mutate(present = ifelse(n > 0, "present", "not_present")) %>%
    count(orig_factor, present)
  congruence6 <- congruence3 %>%
    full_join(congruence5) %>%
    mutate(size = nn/10000)
  
  p <- ggplot(congruence2,
              aes(x = orig_factor, y = congruence, color = orig_factor)) +
    geom_hline(aes(yintercept = 0.9), lty = 3) +
    geom_point(position = position_jitter(width = 0.4, height = 0),
               alpha = 0.3) +
    geom_pointrange(data = congruence6,
                    aes(x = orig_factor, y = Mean,
                        ymin = Lower, ymax = Upper),
                        #size = as.numeric(size)),
                    size = 1,
                    shape = 15) +
    # annotate("text", x = 3, y = 0.89, 
    #          label = "congurance considered 'high'") +
    geom_text(data = congruence5,
              aes(x = orig_factor, y = 0,
                  label = paste0("n = ", nn, " (", 
                                 nn/max(congruence2$iter)*100, "%)")),
              vjust = 1) +
    scale_color_brewer(palette = "Set1") +
    # scale_size_area() +
    theme_bw() +
    theme(legend.position = "none") +
    ylim(-0.05, 1) +
    labs(#title = "highest congruence score by factor and iteration",
      subtitle = paste0(prop_rep * 100, "% noise (", 
                        max(congruence2$iter), " iterations)"),
      x = "original factor",
      y = "congruence")
  
  return(p)
}

# congruence -----

# replace 0% of 7-9yo responses
d00.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
                          prop_rep = .00, niter = 100)
p00.100 <- plot_fun(d00.100, prop_rep = .00) +
  geom_point(data = young_cong, 
             aes(x = orig_factor, y = congruence, color = factor), 
             size = 3, shape = 3, stroke = 2) + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))
  
# replace 10% of 7-9yo responses
d10.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
                          prop_rep = .10, niter = 100)
p10.100 <- plot_fun(d10.100, prop_rep = .10) +
  geom_point(data = young_cong, 
             aes(x = orig_factor, y = congruence, color = factor), 
             size = 3, shape = 3, stroke = 2) + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# # replace 12% of 7-9yo responses
# d12.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
#                           prop_rep = .12, niter = 100)
# p12.100 <- plot_fun(d12.100, prop_rep = .12) +
#   geom_point(data = young_cong, 
#              aes(x = orig_factor, y = congruence, color = factor), 
#              size = 3, shape = 3, stroke = 2) + 
#   scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))
# 
# # replace 14% of 7-9yo responses
# d14.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
#                           prop_rep = .14, niter = 100)
# p14.100 <- plot_fun(d14.100, prop_rep = .14) +
#   geom_point(data = young_cong, 
#              aes(x = orig_factor, y = congruence, color = factor), 
#              size = 3, shape = 3, stroke = 2) + 
#   scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))
# 
# # replace 16% of 7-9yo responses
# d16.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
#                           prop_rep = .16, niter = 100)
# p16.100 <- plot_fun(d16.100, prop_rep = .16) +
#   geom_point(data = young_cong, 
#              aes(x = orig_factor, y = congruence, color = factor), 
#              size = 3, shape = 3, stroke = 2) + 
#   scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))
# 
# # replace 18% of 7-9yo responses
# d18.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
#                           prop_rep = .18, niter = 100)
# p18.100 <- plot_fun(d18.100, prop_rep = .18) +
#   geom_point(data = young_cong, 
#              aes(x = orig_factor, y = congruence, color = factor), 
#              size = 3, shape = 3, stroke = 2) + 
#   scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 20% of 7-9yo responses
d20.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
                          prop_rep = .20, niter = 100)
p20.100 <- plot_fun(d20.100, prop_rep = .20) +
  geom_point(data = young_cong, 
             aes(x = orig_factor, y = congruence, color = factor), 
             size = 3, shape = 3, stroke = 2) + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 30% of 7-9yo responses
d30.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
                          prop_rep = .30, niter = 100)
p30.100 <- plot_fun(d30.100, prop_rep = .30) +
  geom_point(data = young_cong, 
             aes(x = orig_factor, y = congruence, color = factor), 
             size = 3, shape = 3, stroke = 2) + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 40% of 7-9yo responses
d40.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
                          prop_rep = .40, niter = 100)
p40.100 <- plot_fun(d40.100, prop_rep = .40) +
  geom_point(data = young_cong, 
             aes(x = orig_factor, y = congruence, color = factor), 
             size = 3, shape = 3, stroke = 2) + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 50% of 7-9yo responses
d50.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
                          prop_rep = .50, niter = 100)
p50.100 <- plot_fun(d50.100, prop_rep = .50) +
  geom_point(data = young_cong, 
             aes(x = orig_factor, y = congruence, color = factor), 
             size = 3, shape = 3, stroke = 2) + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 60% of 7-9yo responses
d60.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
                          prop_rep = .60, niter = 100)
p60.100 <- plot_fun(d60.100, prop_rep = .60) +
  geom_point(data = young_cong, 
             aes(x = orig_factor, y = congruence, color = factor), 
             size = 3, shape = 3, stroke = 2) + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 70% of 7-9yo responses
d70.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
                          prop_rep = .70, niter = 100)
p70.100 <- plot_fun(d70.100, prop_rep = .70) +
  geom_point(data = young_cong, 
             aes(x = orig_factor, y = congruence, color = factor), 
             size = 3, shape = 3, stroke = 2) + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# # replace 80% of 7-9yo responses
# d80.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20,
#                           prop_rep = .80, niter = 100)
# p80.100 <- plot_fun(d80.100, prop_rep = .80)
# 
# # replace 90% of 7-9yo responses
# d90.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
#                           prop_rep = .90, niter = 100)
# p90.100 <- plot_fun(d90.100, prop_rep = .90)
# 
# # replace 100% of 7-9yo responses
# d100.100 <- congruence_fun(df = d3_all, orig_efa = d3_orig_efa, nvar = 20, 
#                           prop_rep = 1, niter = 100)
# p100.100 <- plot_fun(d100.100, prop_rep = 1)

# plot_grid(p00.100, p10.100, p20.100, p30.100, p40.100, p50.100)
# plot_grid(p50.100, p40.100, p30.100, p20.100, p10.100, p00.100)
plot_grid(p00.100, p10.100, p20.100, p30.100, 
          p40.100, p50.100, p60.100, p70.100, ncol = 4)

# plot_grid(p10.100, p12.100, p14.100, p16.100, p18.100, p20.100, ncol = 4)

# loadings -----

# # do efa on noisy data
# efa10_hb <- fa_fun_hb(replace_nprop_fun(df = d3_all, prop_rep = .10))
# efa20_hb <- fa_fun_hb(replace_nprop_fun(df = d3_all, prop_rep = .20))
# efa30_hb <- fa_fun_hb(replace_nprop_fun(df = d3_all, prop_rep = .30))
# 
# efa10_pa <- fa_fun_pa(replace_nprop_fun(df = d3_all, prop_rep = .10))
# efa20_pa <- fa_fun_pa(replace_nprop_fun(df = d3_all, prop_rep = .20))
# efa30_pa <- fa_fun_pa(replace_nprop_fun(df = d3_all, prop_rep = .30))
# 
# efa10_BIC <- fa_fun_BIC(replace_nprop_fun(df = d3_all, prop_rep = .10))
# efa20_BIC <- fa_fun_BIC(replace_nprop_fun(df = d3_all, prop_rep = .20))
# efa30_BIC <- fa_fun_BIC(replace_nprop_fun(df = d3_all, prop_rep = .30))

# make function to do efa many times on noisy data
efa_many_fun <- function(df, orig_efa, prop_rep, niter) {
  many <- data.frame(capacity = character(),
                     factor = character(),
                     loading = numeric(),
                     orig_factor = character(),
                     congruence = numeric(),
                     iter = numeric())
  
  for(i in 1:niter) {
    data <- replace_nprop_fun(df, prop_rep)

    efa <- fa_fun(data)$loadings[] 
    
    loadings <- efa %>%
      data.frame() %>%
      rownames_to_column("capacity") %>%
      gather(factor, loading, -capacity)
    
    cong <- fa.congruence(efa, orig_efa) %>%
      data.frame() %>%
      rownames_to_column("factor") %>%
      gather(orig_factor, congruence, -factor) %>%
      group_by(factor) %>%
      top_n(1, congruence)
    
    res <- full_join(loadings, cong) %>%
      mutate(iter = as.numeric(i)) %>%
      data.frame()
    
    many <- full_join(many, res)
  }
  
  return(many)
}

# make function to plot factor loadings
efa_many_plot_fun <- function(df_many, prop_rep) {
  boot <- df_many %>%
    group_by(orig_factor, capacity) %>%
    do(data.frame(rbind(smean.cl.boot(.$loading))))
  
  order <- boot %>%
    group_by(capacity) %>%
    top_n(1, abs(Mean)) %>%
    ungroup() %>%
    data.frame() %>%
    arrange(orig_factor, desc(Mean)) %>%
    rownames_to_column("order") %>%
    mutate(order = as.numeric(order)) %>%
    rename(dom_factor = orig_factor) %>%
    select(capacity, order, dom_factor)
  
  many <- df_many %>%
    # group_by(iter, capacity) %>%
    # top_n(1, abs(loading)) %>%
    # ungroup() %>%
    # distinct() %>%
    rename(dom_loading = loading) %>%
    select(capacity, orig_factor, dom_loading, iter) %>%
    full_join(order)
  
  df <- full_join(boot, order)
  
  p <- ggplot(df,
              aes(x = reorder(capacity, desc(order)), y = Mean,
                  color = dom_factor)) +
    facet_wrap(~ orig_factor) + 
    geom_point(data = many,
               aes(x = reorder(capacity, desc(order)), 
                   y = dom_loading, color = dom_factor),
               alpha = 0.1,
               position = position_jitter(width = 0.4, height = 0)) +
    geom_hline(yintercept = 0, lty = 3) +
    geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
    scale_color_brewer("dominant factor (by congruence)", palette = "Set1") +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(subtitle = paste0(prop_rep*100, "% noise (", 
                        max(df_many$iter), " iterations)"),
         x = "capacity",
         y = "mean factor loading") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
  
  return(p)
}

# replace 00% of 7-9yo responses
efa00.100 <- efa_many_fun(df = d3_all, orig_efa = d3_orig_efa,
                          prop_rep = .00, niter = 100)
pefa00.00 <- efa_many_plot_fun(df_many = efa00.100, prop_rep = .00) +
  geom_point(data = d3_young_efa %>%
               data.frame() %>%
               rownames_to_column("capacity") %>%
               gather(factor, loading, -capacity) %>%
               group_by(capacity) %>%
               top_n(1, abs(loading)),
             aes(x = capacity,
                 y = loading,
                 color = factor),
             size = 2, shape = 3, stroke = 2) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 10% of 7-9yo responses
efa10.100 <- efa_many_fun(df = d3_all, orig_efa = d3_orig_efa,
                      prop_rep = .10, niter = 100)
pefa10.00 <- efa_many_plot_fun(df_many = efa10.100, prop_rep = .10) +
  geom_point(data = d3_young_efa %>%
               data.frame() %>%
               rownames_to_column("capacity") %>%
               gather(factor, loading, -capacity) %>%
               group_by(capacity) %>%
               top_n(1, abs(loading)),
             aes(x = capacity,
                 y = loading,
                 color = factor),
             size = 2, shape = 3, stroke = 2) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 20% of 7-9yo responses
efa20.100 <- efa_many_fun(df = d3_all, orig_efa = d3_orig_efa,
                          prop_rep = .20, niter = 100)
pefa20.00 <- efa_many_plot_fun(df_many = efa20.100, prop_rep = .20) +
  geom_point(data = d3_young_efa %>%
               data.frame() %>%
               rownames_to_column("capacity") %>%
               gather(factor, loading, -capacity) %>%
               group_by(capacity) %>%
               top_n(1, abs(loading)),
             aes(x = capacity,
                 y = loading,
                 color = factor),
             size = 2, shape = 3, stroke = 2) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 30% of 7-9yo responses
efa30.100 <- efa_many_fun(df = d3_all, orig_efa = d3_orig_efa,
                          prop_rep = .30, niter = 100)
pefa30.00 <- efa_many_plot_fun(df_many = efa30.100, prop_rep = .30) +
  geom_point(data = d3_young_efa %>%
               data.frame() %>%
               rownames_to_column("capacity") %>%
               gather(factor, loading, -capacity) %>%
               group_by(capacity) %>%
               top_n(1, abs(loading)),
             aes(x = capacity,
                 y = loading,
                 color = factor),
             size = 2, shape = 3, stroke = 2) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 40% of 7-9yo responses
efa40.100 <- efa_many_fun(df = d3_all, orig_efa = d3_orig_efa,
                          prop_rep = .40, niter = 100)
pefa40.00 <- efa_many_plot_fun(df_many = efa40.100, prop_rep = .40) +
  geom_point(data = d3_young_efa %>%
               data.frame() %>%
               rownames_to_column("capacity") %>%
               gather(factor, loading, -capacity) %>%
               group_by(capacity) %>%
               top_n(1, abs(loading)),
             aes(x = capacity,
                 y = loading,
                 color = factor),
             size = 2, shape = 3, stroke = 2) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 50% of 7-9yo responses
efa50.100 <- efa_many_fun(df = d3_all, orig_efa = d3_orig_efa,
                          prop_rep = .50, niter = 100)
pefa50.00 <- efa_many_plot_fun(df_many = efa50.100, prop_rep = .50) +
  geom_point(data = d3_young_efa %>%
               data.frame() %>%
               rownames_to_column("capacity") %>%
               gather(factor, loading, -capacity) %>%
               group_by(capacity) %>%
               top_n(1, abs(loading)),
             aes(x = capacity,
                 y = loading,
                 color = factor),
             size = 2, shape = 3, stroke = 2) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 60% of 7-9yo responses
efa60.100 <- efa_many_fun(df = d3_all, orig_efa = d3_orig_efa,
                          prop_rep = .60, niter = 100)
pefa60.00 <- efa_many_plot_fun(df_many = efa60.100, prop_rep = .60) +
  geom_point(data = d3_young_efa %>%
               data.frame() %>%
               rownames_to_column("capacity") %>%
               gather(factor, loading, -capacity) %>%
               group_by(capacity) %>%
               top_n(1, abs(loading)),
             aes(x = capacity,
                 y = loading,
                 color = factor),
             size = 2, shape = 3, stroke = 2) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

# replace 70% of 7-9yo responses
efa70.100 <- efa_many_fun(df = d3_all, orig_efa = d3_orig_efa,
                          prop_rep = .70, niter = 100)
pefa70.00 <- efa_many_plot_fun(df_many = efa70.100, prop_rep = .70) +
  geom_point(data = d3_young_efa %>%
               data.frame() %>%
               rownames_to_column("capacity") %>%
               gather(factor, loading, -capacity) %>%
               group_by(capacity) %>%
               top_n(1, abs(loading)),
             aes(x = capacity,
                 y = loading,
                 color = factor),
             size = 2, shape = 3, stroke = 2) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a"))

plot_grid(pefa00.00, pefa10.00, pefa20.00, pefa30.00, 
          pefa40.00, pefa50.00, pefa60.00, #pefa70.00,
          ncol = 4)
