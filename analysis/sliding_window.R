# first run dimkid_cogsci_analysis.Rmd
library(ggrepel)
# library(directlabels)
library(gganimate)

# data prep -----

# merge datasets from studies 3-4 and run 04
d_slide <- d3 %>%
  select(-trial.comments) %>%
  full_join(d4 %>% select(-trial.comments)) %>%
  full_join(d5 %>% select(-trial.comments))

# get age ranks
d_slide_subid <- d_slide %>%
  distinct(subid, age) %>%
  arrange(age, subid) %>%
  rownames_to_column("age_rank") %>%
  mutate(age_rank = as.numeric(age_rank))

d_slide <- d_slide %>%
  filter(subid %in% d_slide_subid$subid) %>%
  left_join(d_slide_subid) %>%
  arrange(age_rank, trialNum) %>%
  filter(!is.na(age))

# look at age distribution

ggplot(d_slide %>% distinct(subid, age),
       aes(x = age)) +
  geom_rect(data = d_slide %>% distinct(subid, age) %>% top_n(1),
            xmin = min(d_slide$age), xmax = max(d_slide$age), ymin = 0, ymax = 12,
            fill = "gray", alpha = 0.5) +
  geom_rect(data = d_slide %>% distinct(subid, age) %>% top_n(1),
            xmin = 5.5, xmax = 7.5, ymin = 0, ymax = 12,
            fill = "red", alpha = 0.5) +
  geom_histogram(binwidth = 3/12) +
  scale_x_continuous("age in years", breaks = seq(4, 10, 1)) +
  theme_bw() + 
  theme(text = element_text(size = 20))

# make functions -----

# for doing efa
fa_fun <- function(df, first_sub, last_sub, n_var = 20) {
  # make window
  data <- df %>%
    filter(subid %in% d_slide_subid$subid[first_sub:last_sub]) %>%
    select(subid, capacity, responseNum) %>%
    spread(capacity, responseNum) %>%
    remove_rownames() %>%
    column_to_rownames("subid")
  
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
                    rotate = chosenRotType, cor = chosenCorType)
  
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
                  alpha = 0.05) # set alpha for RMSEA
  
  return(efa_final)
}

# for multiplot (from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# set window size -----

window_size <- 120

# loop over sliding window -----

# get the "right" number of factors
all_efa <- list(NULL)
for(i in 1:(length(d_slide_subid$subid)-window_size)) {
  efa_temp <- fa_fun(d_slide, first_sub = i, last_sub = i+window_size-1, n_var = 20)
  all_efa[[i]] <- efa_temp
}

# force 1 factor
all_efa_1 <- list(NULL)
for(i in 1:(length(d_slide_subid$subid)-window_size)) {
  d <- d_slide %>%
    filter(subid %in% d_slide_subid[i:(i+window_size-1),"subid"]) %>%
    select(subid, capacity, responseNum) %>%
    spread(capacity, responseNum) %>%
    remove_rownames() %>%
    column_to_rownames("subid")
  efa_temp <- fa(d, nfactors = 1, rotate = "oblimin")
  all_efa_1[[i]] <- efa_temp
}

# force 2 factors
all_efa_2 <- list(NULL)
for(i in 1:(length(d_slide_subid$subid)-window_size)) {
  d <- d_slide %>%
    filter(subid %in% d_slide_subid[i:(i+window_size-1),"subid"]) %>%
    select(subid, capacity, responseNum) %>%
    spread(capacity, responseNum) %>%
    remove_rownames() %>%
    column_to_rownames("subid")
  efa_temp <- fa(d, nfactors = 2, rotate = "oblimin")
  all_efa_2[[i]] <- efa_temp
}

# force 3 factors
all_efa_3 <- list(NULL)
for(i in 1:(length(d_slide_subid$subid)-window_size)) {
  d <- d_slide %>%
    filter(subid %in% d_slide_subid[i:(i+window_size-1),"subid"]) %>%
    select(subid, capacity, responseNum) %>%
    spread(capacity, responseNum) %>%
    remove_rownames() %>%
    column_to_rownames("subid")
  efa_temp <- fa(d, nfactors = 3, rotate = "oblimin")
  all_efa_3[[i]] <- efa_temp
}

# get min, max, mean, median age -----

ages <- NULL
for(i in 1:(length(d_slide_subid$subid)-window_size)) {
  subids <- d_slide_subid[i:(i+window_size-1),]$subid
  d_win <- d_slide %>% filter(subid %in% subids)
  min_age <- min(d_win$age)
  max_age <- max(d_win$age)
  mean_age <- mean(d_win$age)
  median_age <- median(d_win$age)
  age_data <- c("min" = min_age, "max" = max_age, 
                "mean" = mean_age, "median" = median_age)
  ages[[i]] <- age_data
}

ages <- data.frame(ages) %>%
  t() %>% 
  data.frame() %>% 
  remove_rownames() %>%
  rownames_to_column("window")

# get nfactors -----

n_factors <- NULL
for(i in 1:length(all_efa)) {
  efa_temp <- c[[i]]
  n_factors[i] <- efa_temp$factors
}

n_factors <- data.frame(n_factors) %>%
  rownames_to_column("window")

# get N strongest-loading (positive) items for each factor -----

topN_items_fun <- function(n_items) {
  topN_items <- NULL
  for(i in 1:length(all_efa)) {
    
    efa_temp <- all_efa[[i]]
    
    loadings <-efa_temp$loadings[] %>%
      data.frame() %>%
      rownames_to_column("capacity") # %>%
    # mutate_at(vars(starts_with("MR")), funs(abs))
    
    MR1_top <- loadings %>% top_n(n_items, MR1)
    MR1_top <- paste(MR1_top$capacity, collapse = ", ")
    
    if("MR2" %in% names(loadings)) {
      MR2_top <- loadings %>% top_n(n_items, MR2)
      MR2_top <- paste(MR2_top$capacity, collapse = ", ")
    } else { MR2_top <- "NA" }
    
    if("MR3" %in% names(loadings)) {
      MR3_top <- loadings %>% top_n(n_items, MR3)
      MR3_top <- paste(MR3_top$capacity, collapse = ", ")
    } else { MR3_top <- "NA" }
    
    if("MR4" %in% names(loadings)) {
      MR4_top <- loadings %>% top_n(n_items, MR4)
      MR4_top <- paste(MR4_top$capacity, collapse = ", ")
    } else { MR4_top <- "NA" }
    
    items <- c("MR1" = MR1_top, "MR2" = MR2_top, "MR3" = MR3_top, "MR4" = MR4_top)
    
    topN_items[[i]] <- items
  }
  
  topN_items <- data.frame(topN_items) %>% 
    t() %>% 
    data.frame() %>% 
    remove_rownames() %>%
    rownames_to_column("window")
  
  return(topN_items)
}
top5_items <- topN_items_fun(5)

# get percent variance explained in final solution for each factor -----

# "right" number of factors
per_var <- list(NULL)
for(i in 1:length(all_efa)) {
  efa_temp <- all_efa[[i]]

  per_var_MR1 <- efa_temp$Vaccounted["Proportion Var","MR1"]

  if("MR2" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR2 <- efa_temp$Vaccounted["Proportion Var","MR2"]
  } else { per_var_MR2 <- "NA" }
  
  if("MR3" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR3 <- efa_temp$Vaccounted["Proportion Var","MR3"]
  } else { per_var_MR3 <- "NA" }
  
  if("MR4" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR4 <- efa_temp$Vaccounted["Proportion Var","MR4"]
  } else { per_var_MR4 <- "NA" }
  
  per_vars <- c("MR1" = per_var_MR1, "MR2" = per_var_MR2, 
             "MR3" = per_var_MR3, "MR4" = per_var_MR4)
  
  per_var[[i]] <- per_vars
}

per_var <- data.frame(per_var) %>%
  t() %>% 
  data.frame() %>% 
  remove_rownames() %>%
  rownames_to_column("window")

# force 1 factor
per_var_1 <- list(NULL)
for(i in 1:length(all_efa)) {
  d <- d_slide %>%
    filter(subid %in% d_slide_subid[i:(i+window_size-1),"subid"]) %>%
    select(subid, capacity, responseNum) %>%
    spread(capacity, responseNum) %>%
    remove_rownames() %>%
    column_to_rownames("subid")
  
  efa_temp <- fa(d, nfactors = 1, rotate = "oblimin")
  
  per_var_MR1 <- efa_temp$Vaccounted["Proportion Var","MR1"]
  
  if("MR2" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR2 <- efa_temp$Vaccounted["Proportion Var","MR2"]
  } else { per_var_MR2 <- "NA" }
  
  if("MR3" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR3 <- efa_temp$Vaccounted["Proportion Var","MR3"]
  } else { per_var_MR3 <- "NA" }
  
  if("MR4" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR4 <- efa_temp$Vaccounted["Proportion Var","MR4"]
  } else { per_var_MR4 <- "NA" }
  
  per_vars <- c("MR1" = per_var_MR1, "MR2" = per_var_MR2, 
                "MR3" = per_var_MR3, "MR4" = per_var_MR4)
  
  per_var_1[[i]] <- per_vars
}

per_var_1 <- data.frame(per_var_1) %>%
  t() %>% 
  data.frame() %>% 
  remove_rownames() %>%
  rownames_to_column("window")

# force 2 factors
per_var_2 <- list(NULL)
for(i in 1:length(all_efa)) {
  d <- d_slide %>%
    filter(subid %in% d_slide_subid[i:(i+window_size-1),"subid"]) %>%
    select(subid, capacity, responseNum) %>%
    spread(capacity, responseNum) %>%
    remove_rownames() %>%
    column_to_rownames("subid")
  
  efa_temp <- fa(d, nfactors = 2, rotate = "oblimin")
  
  per_var_MR1 <- efa_temp$Vaccounted["Proportion Var","MR1"]
  
  if("MR2" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR2 <- efa_temp$Vaccounted["Proportion Var","MR2"]
  } else { per_var_MR2 <- "NA" }
  
  if("MR3" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR3 <- efa_temp$Vaccounted["Proportion Var","MR3"]
  } else { per_var_MR3 <- "NA" }
  
  if("MR4" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR4 <- efa_temp$Vaccounted["Proportion Var","MR4"]
  } else { per_var_MR4 <- "NA" }
  
  per_vars <- c("MR1" = per_var_MR1, "MR2" = per_var_MR2, 
                "MR3" = per_var_MR3, "MR4" = per_var_MR4)
  
  per_var_2[[i]] <- per_vars
}

per_var_2 <- data.frame(per_var_2) %>%
  t() %>% 
  data.frame() %>% 
  remove_rownames() %>%
  rownames_to_column("window")

# force 3 factors
per_var_3 <- list(NULL)
for(i in 1:length(all_efa)) {
  d <- d_slide %>%
    filter(subid %in% d_slide_subid[i:(i+window_size-1),"subid"]) %>%
    select(subid, capacity, responseNum) %>%
    spread(capacity, responseNum) %>%
    remove_rownames() %>%
    column_to_rownames("subid")
  
  efa_temp <- fa(d, nfactors = 3, rotate = "oblimin")
  
  per_var_MR1 <- efa_temp$Vaccounted["Proportion Var","MR1"]
  
  if("MR2" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR2 <- efa_temp$Vaccounted["Proportion Var","MR2"]
  } else { per_var_MR2 <- "NA" }
  
  if("MR3" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR3 <- efa_temp$Vaccounted["Proportion Var","MR3"]
  } else { per_var_MR3 <- "NA" }
  
  if("MR4" %in% names(data.frame(efa_temp$loadings[]))) {
    per_var_MR4 <- efa_temp$Vaccounted["Proportion Var","MR4"]
  } else { per_var_MR4 <- "NA" }
  
  per_vars <- c("MR1" = per_var_MR1, "MR2" = per_var_MR2, 
                "MR3" = per_var_MR3, "MR4" = per_var_MR4)
  
  per_var_3[[i]] <- per_vars
}

per_var_3 <- data.frame(per_var_3) %>%
  t() %>% 
  data.frame() %>% 
  remove_rownames() %>%
  rownames_to_column("window")

per_var_all <- per_var %>% mutate(factors = "n") %>%
  full_join(per_var_1 %>% mutate(factors = "1")) %>%
  full_join(per_var_2 %>% mutate(factors = "2")) %>%
  full_join(per_var_3 %>% mutate(factors = "3")) %>%
  mutate_at(vars(window, starts_with("MR")), funs(as.numeric)) %>%
  select(-MR4) %>%
  gather(factor, per_var, starts_with("MR")) %>%
  # mutate(per_var = ifelse(is.na(per_var), 0, per_var)) %>%
  group_by(window, factors) %>%
  summarise(total_var_exp = sum(per_var, na.rm = T))
  
# get RMSEA in final solution for each factor -----

# "right" number of factors
rmsea <- NULL
for(i in 1:length(all_efa)) {
  efa_temp <- all_efa[[i]]
  rmsea_temp <- efa_temp$RMSEA
  rmsea <- rbind(rmsea, rmsea_temp)
}

rmsea <- data.frame(rmsea) %>%
  remove_rownames() %>%
  rownames_to_column("window")

# force 1 factor
rmsea_1 <- NULL
for(i in 1:length(all_efa)) {
  efa_temp <- all_efa[[i]]
  rmsea_temp <- efa_temp$RMSEA
  rmsea_1 <- rbind(rmsea_1, rmsea_temp)
}

rmsea_1 <- data.frame(rmsea_1) %>%
  remove_rownames() %>%
  rownames_to_column("window")

# force 2 factors
rmsea_2 <- NULL
for(i in 1:length(all_efa)) {
  efa_temp <- all_efa[[i]]
  rmsea_temp <- efa_temp$RMSEA
  rmsea_2 <- rbind(rmsea_2, rmsea_temp)
}

rmsea_2 <- data.frame(rmsea_2) %>%
  remove_rownames() %>%
  rownames_to_column("window")

# force 3 factors
rmsea_3 <- NULL
for(i in 1:length(all_efa)) {
  efa_temp <- all_efa[[i]]
  rmsea_temp <- efa_temp$RMSEA
  rmsea_3 <- rbind(rmsea_3, rmsea_temp)
}

rmsea_3 <- data.frame(rmsea_3) %>%
  remove_rownames() %>%
  rownames_to_column("window")

all_rmsea <- rmsea %>% mutate(factors = "n") %>%
  full_join(rmsea_1 %>% mutate(factors = "1")) %>%
  full_join(rmsea_2 %>% mutate(factors = "2")) %>%
  full_join(rmsea_3 %>% mutate(factors = "3"))
  

# code as BHM -----

# loop it
congruence <- data.frame(window = numeric(), 
                         MR1 = character(),
                         MR2 = character(),
                         MR3 = character())
loadings_final <- all_efa[[length(all_efa)]]$loadings[] %>%
  data.frame() %>%
  rename(HEART = MR3, BODY = MR1, MIND = MR2) %>%
  as.matrix()
for(i in 1:length(all_efa)) {
  efa_temp <- all_efa[[i]]$loadings[]
  cong_temp <- factor.congruence(efa_temp, loadings_final) %>%
    data.frame() %>%
    rownames_to_column("earlier") %>%
    gather(later, congruence, -earlier) %>%
    group_by(earlier) %>%
    top_n(1, congruence)
  
  cong_temp2 <- cong_temp %>% 
    ungroup() %>% 
    group_by(earlier) %>% 
    mutate(later = glue::collapse(later, sep = "-")) %>%
    # mutate(later = c(cong_temp$later) %>% 
    #          glue::collapse(sep = "-")) %>% 
    ungroup() %>% 
    distinct()
  
  cong_temp3 <- cong_temp2 %>%
    select(-congruence) %>%
    mutate(window = as.numeric(as.character(i))) %>%
    spread(earlier, later)
  
  congruence <- full_join(congruence, cong_temp3)
}

congruence <- congruence %>% 
  gather(factor, label, -window) %>%
  filter(!is.na(label)) %>%
  mutate(window = as.character(window))
  

# put them all together -----
all_data <- ages %>%
  full_join(n_factors) %>%
  full_join(top5_items %>% 
              gather(factor, top5, -window) %>% 
              filter(!is.na(top5), top5 != "NA")) %>%
  full_join(per_var %>% 
              gather(factor, per_var, -window) %>% 
              filter(!is.na(per_var), per_var != "NA")) %>%
  full_join(congruence) %>%
  mutate(window = as.numeric(as.character(window)),
         per_var = as.numeric(as.character(per_var)))

# plots of loadings -----

loading_plots <- list(NULL)
oldest_kid_factors <- all_efa[[length(all_efa)]]$loadings[] %>%
  data.frame() %>%
  rownames_to_column("capacity") %>%
  gather(factor, loading, starts_with("MR")) %>%
  group_by(capacity) %>%
  top_n(1, abs(loading)) %>%
  select(-loading) %>%
  rename(old_dom = factor)

# # approach #1
# for(i in 1:length(all_efa)) {
#   
#   efa_temp <- all_efa[[i]]
#   
#   loadings_temp <- efa_temp$loadings[] %>%
#     data.frame() %>%
#     rownames_to_column("capacity") %>%
#     gather(factor, loading, starts_with("MR")) %>%
#     full_join(oldest_kid_factors) %>%
#     mutate(old_dom = factor(old_dom, 
#                             levels = c("MR1", "MR3", "MR2"),
#                             labels = c("BODY", "HEART", "MIND")))
#   
#   g <- ggplot(loadings_temp %>% gather(factor, loading, starts_with("MR")),
#               aes(x = factor, y = loading, label = capacity, color = old_dom)) +
#     ggrepel::geom_text_repel() +
#     scale_color_brewer(# name = "color-coded by dominant factor among oldest children: ",
#                        palette = "Set1") +
#     scale_y_continuous(limits = c(-1, 1)) +
#     # theme(legend.position = "top") +
#     theme_minimal() +
#     theme(legend.position = "none",
#           panel.border = element_rect(colour = "black", fill=NA, size=1),
#           axis.title = element_blank()) +
#     labs(title = paste("WINDOW", i))
#   
#   loading_plots[[i]] <- g
# 
# }
# # gridExtra::grid.arrange(loading_plots[[1]], loading_plots[[2]], loading_plots[[3]], 
# #                         loading_plots[[4]], loading_plots[[5]], loading_plots[[6]],
# #                         nrow = 2)
# 
# approach #2

all_loadings <- NULL

for(i in 1:length(all_efa)) {

  efa_temp <- all_efa[[i]]

  loadings_temp <- efa_temp$loadings[] %>%
    data.frame() %>%
    rownames_to_column("capacity") %>%
    gather(factor, loading, starts_with("MR")) %>%
    mutate(window = i)

  all_loadings <- rbind(all_loadings, loadings_temp)

}

all_loadings <- all_loadings %>%
  full_join(oldest_kid_factors) %>%
  mutate(old_dom = factor(old_dom,
                          levels = c("MR1", "MR3", "MR2"),
                          labels = c("BODY", "HEART", "MIND")))
  full_join(all_data) %>%
  select(window, min, max, mean, median, n_factors, 
         factor, label, top5, per_var, 
         capacity, loading) %>%
  mutate_at(vars(window, min, max, mean, median, n_factors, per_var, loading), 
            funs(as.numeric))

# ggplot(all_loadings,
#        aes(x = factor, y = loading, label = capacity, color = old_dom)) +
#   facet_wrap(~ window, ncol = 11, scales = "fixed") +
#   # ggrepel::geom_text_repel() +
#   geom_text(size = 4) +
#   scale_color_brewer( name = "color-coded by dominant factor among oldest children: ",
#     palette = "Set1") +
#   # scale_y_continuous(limits = c(-1, 1)) +
#   theme_minimal() +
#   theme(legend.position = "none",
#         text = element_text(size = 10))
#   # theme(legend.position = "none",
#   #       panel.border = element_rect(colour = "black", fill=NA, size=1),
#   #       axis.title = element_blank())
# 
# ggplot(all_loadings,
#        aes(x = factor, y = loading, label = capacity, color = old_dom)) +
#   facet_wrap(~ window, nrow = 5, scales = "fixed") +
#   # ggrepel::geom_text_repel() +
#   geom_point(size = 6, position = "jitter") +
#   scale_color_brewer( name = "color-coded by dominant factor among oldest children: ",
#                       palette = "Set1") +
#   # scale_y_continuous(limits = c(-1, 1)) +
#   # theme_minimal() +
#   theme_bw() +
#   theme(legend.position = "none",
#         text = element_text(size = 20))
# # theme(legend.position = "none",
# #       panel.border = element_rect(colour = "black", fill=NA, size=1),
# #       axis.title = element_blank()) # 6000 by 3000


# trying to plot dominant loading
all_dom <- all_loadings %>%
  group_by(window, capacity) %>%
  top_n(1, abs(loading)) %>%
  # mutate(factor_num = factor(factor, levels = c("MR2", "MR1", "MR3")),
  #        combo = as.numeric(factor_num) + abs(loading) - 1) %>%
  mutate(factor_num = recode(label,
                             "HEART-BODY" = 2,
                             "BODY" = 1,
                             "HEART" = 2,
                             "MIND" = 3),
         combo = as.numeric(factor_num) + abs(loading) - 1) %>%
  full_join(all_data %>% mutate(window = as.numeric(window)))

domovertime <- ggplot(all_dom, aes(x = median, y = combo, 
                    # color = label, 
                    color = old_dom, 
                    group = capacity, label = capacity, frame = median)) +
  geom_point(aes(group = capacity, cumulative = TRUE), size = 4) +
  geom_line(aes(group = capacity, cumulative = TRUE), alpha = 0.5) +
  theme_bw() +
  scale_x_continuous(name = "median age in years (by window)", 
                     limits = c(all_dom$median[all_dom$window == 2 & 
                                                 !is.na(all_dom$median)][1] - 0.5, 
                                all_dom$median[all_dom$window == max(all_dom$window) & 
                                                 !is.na(all_dom$median)][1] + 0.5)) +
  scale_y_continuous(name = "factor + loading (higher = stronger)") +
  # scale_color_manual(name = "dominant factor loading in final window: ",
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#4daf4a")) +
  scale_color_brewer(name = "dominant factor loading in final window: ",
                     palette = "Set1") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size = 30),
        legend.position = "none")
        # legend.position = "top") # 2000 by 1500

domovertime +
  geom_label_repel(data = all_dom %>% filter(window == min(all_dom$window)), size = 7,
                   segment.color = "black", segment.size = 0.4,
                   xlim = c(all_dom$median[all_dom$window == min(all_dom$window) &
                                             !is.na(all_dom$median)][1] - 0.2,
                            all_dom$median[all_dom$window == min(all_dom$window) &
                                             !is.na(all_dom$median)][1] - 0.1)) +
  geom_label_repel(data = all_dom %>% filter(window == 20), size = 7,
                   segment.color = "black", segment.size = 0.4,
                   xlim = c(all_dom$median[all_dom$window == 20 &
                                             !is.na(all_dom$median)][1] + 0.1,
                            all_dom$median[all_dom$window == 20 &
                                             !is.na(all_dom$median)][1] + 0.2)) +
  geom_label_repel(data = all_dom %>% filter(window == 88), size = 7,
                   segment.color = "black", segment.size = 0.4,
                   xlim = c(all_dom$median[all_dom$window == 88 &
                                             !is.na(all_dom$median)][1] - 0.2,
                            all_dom$median[all_dom$window == 88 &
                                             !is.na(all_dom$median)][1] - 0.1)) +
  geom_label_repel(data = all_dom %>% filter(window == 92), size = 7,
                   segment.color = "black", segment.size = 0.4,
                   xlim = c(all_dom$median[all_dom$window == 92 &
                                             !is.na(all_dom$median)][1] + 0.1,
                            all_dom$median[all_dom$window == 92 &
                                             !is.na(all_dom$median)][1] + 0.2)) +
  geom_label_repel(data = all_dom %>% filter(window == 155), size = 7, 
                   segment.color = "black", segment.size = 0.4,
                   xlim = c(all_dom$median[all_dom$window == 155 & 
                                             !is.na(all_dom$median)][1] - 0.2, 
                            all_dom$median[all_dom$window == max(all_dom$window) & 
                                             !is.na(all_dom$median)][1] - 0.1)) +
  geom_label_repel(data = all_dom %>% filter(window == max(all_dom$window)), size = 7, 
                   segment.color = "black", segment.size = 0.4,
                   xlim = c(all_dom$median[all_dom$window == max(all_dom$window) & 
                                             !is.na(all_dom$median)][1] + 0.1, 
                            all_dom$median[all_dom$window == max(all_dom$window) & 
                                             !is.na(all_dom$median)][1] + 0.2))

gganimate(domovertime, 
          "domovertime.gif",
          title_frame = FALSE,
          interval=0.1,
          ani.width=2000, ani.height=1500)

# plot of % var explained -----

var_plot <- per_var %>% 
  mutate_all(funs(as.character)) %>%
  mutate_all(funs(as.numeric)) %>%
  gather(factor, per_var, -window) %>%
  left_join(all_data %>% 
              filter(!is.na(median)) %>%
              mutate(window = as.numeric(window)) %>% 
              distinct(window, median))

ggplot(var_plot %>%
         full_join(var_plot %>%
                     group_by(window) %>%
                     summarise(total_var_expl = sum(per_var, na.rm = T)) %>%
                     ungroup() %>%
                     distinct(window, factor, per_var, total_var_expl)) %>%
         filter(factor != "MR4") %>%
         mutate(factor = factor(as.character(factor))), 
       aes(x = median, y = per_var, color = factor)) +
  geom_point(size = 4) +
  geom_line(alpha = 0.8) +
  geom_point(aes(y = total_var_expl), color = "gray", size = 4) +
  geom_line(aes(y = total_var_expl), color = "gray", alpha = 0.8) +
  theme_bw() +
  scale_x_continuous(name = "median age in years (by window)", 
                     breaks = seq(5, 10, 1),
                     limits = c(floor(all_dom$median[all_dom$window == min(all_dom$window) & !is.na(all_dom$median)][1]),
                                ceiling(all_dom$median[all_dom$window == max(all_dom$window) & !is.na(all_dom$median)][1]))) +
  scale_y_continuous(name = "proportion variance explained", limits = c(0, 1)) +
  scale_color_brewer(name = "factor: ",
                     palette = "Set2") +
  theme(#axis.text.y = element_blank(), 
        #axis.ticks.y = element_blank(),
        # panel.grid.major.y = element_blank(),
        # panel.grid.minor.y = element_blank(),
        text = element_text(size = 20),
        legend.position = "right")
# legend.position = "top") # 2000 by 1500

ggplot(per_var_all %>%
         spread(factors, total_var_exp) %>%
         mutate(best = ifelse(`1` == n, 1,
                              ifelse(`2` == n, 2,
                                     ifelse(`3` == n, 3,
                                            NA)))) %>%
         select(-n) %>%
         gather(factors, total_var_exp, c(`1`, `2`, `3`)) %>%
         mutate(factors = as.numeric(as.character(factors)),
                best_cat = ifelse(factors == best, "yes", "no")) %>%
         mutate_at(vars(factors, best_cat), funs(factor)) %>%
         left_join(all_data %>% 
                     select(window, median) %>% 
                     mutate(window = as.numeric(window))),
       aes(x = median, y = total_var_exp, 
           color = factors, fill  = best_cat, group = factors)) +
  geom_smooth(alpha = 0.2) +
  geom_line(alpha = 0.8) +
  geom_point(shape = 21) +
  scale_fill_manual(values = c("white", "black")) +
  scale_x_continuous(name = "median age in years (by window)", 
                     breaks = seq(5, 10, 1),
                     limits = c(floor(all_dom$median[all_dom$window == min(all_dom$window) & !is.na(all_dom$median)][1]),
                                ceiling(all_dom$median[all_dom$window == max(all_dom$window) & !is.na(all_dom$median)][1]))) +
  scale_y_continuous(name = "TOTAL variance explained") + #, limits = c(0, 1)) +
  labs(fill = "best solution?: ", color = "number of factors: ") +
  theme_bw()

ggplot(per_var_all %>%
         spread(factors, total_var_exp) %>%
         mutate(two.v.one = `2` - `1`,
                three.v.two = `3` - `2`) %>%
         gather(contrast, diff, c(two.v.one, three.v.two)),
       aes(x = window, y = diff, color = contrast)) +
  geom_point() +
  geom_line(alpha = 0.8) +
  theme_bw()

# plot of RMSEA -----

# rmsea_plot <- all_rmsea %>% 
#   mutate(window = as.numeric(as.character(window))) %>%
#   left_join(all_data %>%
#               filter(!is.na(median)) %>%
#               mutate(window = as.numeric(window)) %>% 
#               distinct(window, median)) %>%
#   mutate(factors = factor(factors))
# 
# ggplot(rmsea_plot, 
#        aes(x = median, y = RMSEA, color = factors, group = factors)) +
#   geom_hline(yintercept = 0.05, lty = 3, color = "blue") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
#   geom_line(alpha = 0.8) +
#   geom_point(size = 4) +
#   theme_bw() +
#   scale_x_continuous(name = "median age in years (by window)") +
#   scale_y_continuous(name = "RMSEA") +
#   theme(#axis.text.y = element_blank(), 
#     #axis.ticks.y = element_blank(),
#     # panel.grid.major.y = element_blank(),
#     # panel.grid.minor.y = element_blank(),
#     text = element_text(size = 20),
#     legend.position = "right")
# # legend.position = "top") # 2000 by 1500
# plot of mean response across windows -----

mean_responses <- NULL
for(i in 1:length(all_efa)) {
  subids <- d_slide_subid[i:(i+119),]
  d <- d_slide %>%
    filter(subid %in% subids$subid)
  boot <- d %>%
    mutate(window = i) %>%
    group_by(window, character, capacity) %>%
    do(data.frame(rbind(smean.cl.boot(.$responseNum))))
  mean_responses <- rbind(mean_responses, boot)
}

mean_responses <- mean_responses %>%
  ungroup() %>%
  full_join(ages %>% mutate(window = as.numeric(as.character(window)))) %>%
  full_join(all_efa[[length(all_efa)]]$loadings[] %>%
              data.frame() %>%
              rownames_to_column("capacity") %>%
              gather(factor, loading, starts_with("MR")) %>%
              group_by(capacity) %>%
              top_n(1, abs(loading)) %>%
              # select(-loading) %>%
              rename(old_dom = factor) %>%
              arrange(old_dom, abs(loading)) %>%
              data.frame() %>%
              rownames_to_column("order") %>%
              mutate(order = as.numeric(as.character(order))))
  
ggplot(mean_responses %>% 
         mutate(capacity = reorder(capacity, order),
                character = factor(character,
                                   levels = c("computer", "robot", "doll", 
                                              "teddy_bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant"))), 
       aes(x = median, y = Mean, color = old_dom, fill = old_dom)) +
  facet_grid(character ~ capacity) +
  geom_hline(yintercept = 0.5, lty = 3, color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.3, lty = 0) +
  geom_line(alpha = 0.8) +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_continuous(name = "median age in years (by window)") +
  scale_y_continuous(name = "mean response (0 = no, 0.5 = kinda, 1 = yes)") +
  theme(#axis.text.y = element_blank(), 
    #axis.ticks.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y = element_blank(),
    text = element_text(size = 20),
    legend.position = "none")

ggplot(mean_responses %>% 
         mutate(capacity = reorder(capacity, order),
                character = factor(character,
                                   levels = c("computer", "robot", "doll", 
                                              "teddy_bear", "beetle", "bird", 
                                              "mouse", "goat", "elephant"))), 
       aes(x = median, y = Mean, color = old_dom, fill = old_dom, group = capacity)) +
  facet_wrap(~ character, ncol = 3) +
  geom_hline(yintercept = 0.5, lty = 3, color = "blue") +
  geom_line(alpha = 0.8) +
  geom_point(size = 2) +
  # geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.02, lty = 0) +
  # geom_label(data = mean_responses %>%
  #              data.frame() %>%
  #              filter(window == max(mean_responses$window)) %>%
  #              distinct(character, capacity, Mean, median, old_dom),
  #            aes(x = median + 0.01, y = Mean, label = capacity),
  #            fill = "white", color = "black") +
  geom_label_repel(data = mean_responses %>%
                     data.frame() %>%
                     filter(window == max(mean_responses$window)) %>%
                     distinct(character, capacity, Mean, median, old_dom), 
                   aes(label = capacity), fill = "white",
                   # size = 7,
                   segment.color = "black", segment.size = 0.1,
                   xlim = c(9, 10)) +
  theme_bw() +
  scale_x_continuous(name = "median age in years (by window)",
                     limits = c(5, 10)) +
  scale_y_continuous(name = "mean response (0 = no, 0.5 = kinda, 1 = yes)") +
  theme(#axis.text.y = element_blank(), 
    #axis.ticks.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y = element_blank(),
    text = element_text(size = 20),
    legend.position = "none")
