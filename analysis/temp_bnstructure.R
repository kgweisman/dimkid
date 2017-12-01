library(tidyverse)
library(bnlearn)
library(BDgraph)
library(igraph)


# bnlearn approach ----- 
# bn1 <- hc(d_slide_all_complete %>% select(-age, -character))

make_ntwk_plot_fun_bnlearn <- function(df) {
  bn1 <- hc(df[complete.cases(df),])
  bn1_arcs <- bn1$arcs %>%
    data.frame()
  
  bn1_arcs_edges <- paste(t(as.matrix(bn1_arcs)))
  
  g1 <- graph(edges = bn1_arcs_edges, directed = F)
  g1_layout <- layout_with_fr(g1)
  
  plot(g1, # edge.arrow.size = .1,
       vertex.size = 15,
       vertex.label.color = "black",
       vertex.label.cex = 0.8, vertex.label.dist = 0, 
       # layout = g1_layout * 0.4,
       rescale = T,
       main = deparse(substitute(df)))
}
make_ntwk_plot_fun_bnlearn(d1_all)
make_ntwk_plot_fun_bnlearn(d2_all)
make_ntwk_plot_fun_bnlearn(d3_all)
make_ntwk_plot_fun_bnlearn(d4_all)

# by hand approach -----

temp <- data.frame()
for(i in 1:length(names(d3_all))) {
  for(j in names(d3_all)[(i+1):length(names(d3_all))]) {
    temp_d3_all <- d3_all %>%
      select(names(d3_all)[i], j)
    temp_diff <- abs(temp_d3_all[2] - temp_d3_all[1])
    temp_sim <- 1 - temp_diff
    temp_sum_sim <- sum(temp_sim, na.rm = T)
    temp[names(d3_all)[i], j] <- temp_sum_sim
  }
}

temp <- data.frame(cap1 = character(),
                   cap2 = character(),
                   sim = numeric())
for(i in 1:length(names(d3_all))) {
  for(j in 1:length(names(d3_all))) {
    temp_d3_all <- cbind(d3_all[,i],
                         d3_all[,j])
    temp_diff <- abs(temp_d3_all[,2] - temp_d3_all[,1])
    temp_sim <- 1 - temp_diff
    temp_sum_sim <- sum(temp_sim, na.rm = T)
    temp <- temp %>%
      full_join(data.frame(cap1 = names(d3_all)[i],
                           cap2 = names(d3_all)[j],
                           sim = temp_sum_sim))
  }
}

temp2 <- temp %>% 
  group_by(cap1, cap2) %>%
  mutate(capA = sort(c(cap1, cap2), decreasing = F)[1],
         capB = ifelse(capA == cap1, cap2, cap1)) %>%
  filter(capA != capB) %>%
  # mutate(edge = paste(capA, capB, sep = ", ")) %>%
  ungroup() %>%
  select(-cap1, -cap2) %>%
  distinct()

temp2_edges <- paste(t(as.matrix(temp2 %>% select(capA, capB))))
g2 <- graph(edges = temp2_edges, directed = F)
g2_layout <- layout_with_fr(g2)


# V(g2)
E(g2)$weight <- temp2$sim
plot(g2, vertex.label.color = "black", 
     edge.width = E(g2)$weight/40)


# BD graph ----

make_ntwk_plot_fun_bdgraph <- function(df, iter = 5000, postprob = .95) {
  bn1 <- bdgraph(df[complete.cases(df),], iter = iter)
  bn1_arcs <- bn1$p_links %>%
    data.frame() %>%
    rownames_to_column("cap1") %>%
    gather(cap2, p, -cap1) %>%
    filter(p > postprob) %>%
    dplyr::select(starts_with("cap"))
  
  bn1_arcs_edges <- paste(t(as.matrix(bn1_arcs)))
  
  g1 <- graph(edges = bn1_arcs_edges, directed = F)
  g1_layout <- layout_with_fr(g1)
  
  plot(g1, # edge.arrow.size = .1,
       vertex.size = 15,
       vertex.label.color = "black",
       vertex.label.cex = 0.8, vertex.label.dist = 0, 
       # layout = g1_layout * 0.4,
       rescale = T,
       main = deparse(substitute(df)))
}
make_ntwk_plot_fun_bdgraph(d1_all, postprob = .95)
make_ntwk_plot_fun_bdgraph(d2_all, postprob = .95)
d3_all_bd_v1 <- make_ntwk_plot_fun_bdgraph(d3_all, iter = 50000, postprob = .95)
# d3_all_bd_v2 <- make_ntwk_plot_fun_bdgraph(d3_all, iter = 50000, postprob = .95)
# d3_all_bd_v3 <- make_ntwk_plot_fun_bdgraph(d3_all, iter = 50000, postprob = .95)
# d3_all_bd_v4 <- make_ntwk_plot_fun_bdgraph(d3_all, iter = 50000, postprob = .95)
# d3_all_bd_v5 <- make_ntwk_plot_fun_bdgraph(d3_all, iter = 50000, postprob = .95)
d4_all_bd_v1 <- make_ntwk_plot_fun_bdgraph(d4_all, iter = 50000, postprob = .95)
# d4_all_bd_v2 <- make_ntwk_plot_fun_bdgraph(d4_all, iter = 50000, postprob = .95)
# d4_all_bd_v3 <- make_ntwk_plot_fun_bdgraph(d4_all, iter = 50000, postprob = .95)
# d4_all_bd_v4 <- make_ntwk_plot_fun_bdgraph(d4_all, iter = 50000, postprob = .95)
# d4_all_bd_v5 <- make_ntwk_plot_fun_bdgraph(d4_all, iter = 50000, postprob = .95)

