grid_plot_fun <- function(df_many, prop_rep) {
  temp <- df_many %>% 
    group_by(orig_factor, capacity) %>% 
    do(data.frame(rbind(smean.cl.boot(.$loading))))
  
  temp_factors <- df_many %>% 
    distinct(iter, orig_factor) %>% #, congruence) %>% 
    # complete(iter, orig_factor) %>%
    group_by(orig_factor) %>%
    count() %>%
    mutate(prop = n/max(df_many$iter)) %>%
    rename(n_iter_present = n,
           prop_iter_present = prop)
  
  temp_order <- temp %>%
    group_by(capacity) %>%
    top_n(1, abs(Mean)) %>%
    arrange(orig_factor, desc(Mean)) %>%
    data.frame() %>%
    rownames_to_column("order") %>%
    mutate(order = as.numeric(as.character(order))) %>%
    select(capacity, order)
  
  temp_cong <- df_many %>%
    group_by(orig_factor) %>%
    do(data.frame(rbind(smean.cl.boot(.$congruence)))) %>%
    select(orig_factor, Mean) %>%
    rename(congruence = Mean)
  
  temp_plot <- full_join(temp, temp_order) %>%
    full_join(temp_cong) %>%
    full_join(temp_factors) %>%
    mutate(factor_lab = paste0(orig_factor, "\n(ave. Rc = ",
                              format(round(congruence, 2), nsmall = 2),
                              ")\n(present in ", 
                              round(prop_iter_present, 2) * 100,
                              "% of iter.)"))
    
  
  ggplot(temp_plot,
         aes(x = factor_lab, y = reorder(capacity, desc(order)), 
             fill = Mean, label = format(round(Mean, 2), nsmall = 2))) +
    geom_tile() +
    geom_text(size = 3) +
    scale_fill_distiller(palette = "RdYlBu",
                         limits = c(-1, 1), breaks = c(-1, 0, 1),
                         guide = guide_colorbar(title = element_blank(),
                                                barheight = 20)) +
    theme_minimal() +
    theme(# axis.text = element_text(size = 12),
          # axis.title = element_blank(),
          text = element_text(size = 12),
          panel.grid = element_blank()) +
    labs(title = "factor loadings",
         subtitle = paste0(prop_rep * 100, "% noise (",
                           max(df_many$iter), " iterations)"),
         x = "\nfactor (by average congruence with 7-9yo factors, Rc)",
         y = "")
}

grid_plot_fun(df_many = efa00.500, prop_rep = .00)
grid_plot_fun(df_many = efa10.500, prop_rep = .10)
grid_plot_fun(df_many = efa20.500, prop_rep = .20)
grid_plot_fun(df_many = efa30.500, prop_rep = .30)
grid_plot_fun(df_many = efa40.500, prop_rep = .40)
grid_plot_fun(df_many = efa50.500, prop_rep = .50)
grid_plot_fun(df_many = efa60.500, prop_rep = .60)
# grid_plot_fun(df_many = efa70.500, prop_rep = .70)

# grid_plot_fun(df_many = d3_young_efa, prop_rep = .00)
efa_young_true <- efa_many_fun(df = d4_all, orig_efa = d3_orig_efa,
                               prop_rep = .00, niter = 500)
grid_plot_fun(df_many = efa_young_true, prop_rep = .00)
