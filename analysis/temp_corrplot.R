library(tidyverse)
library(corrplot)

# d_slide_all_complete %>%
#   dist() %>%
#   hclust() %>%
#   plot()

make_corrplots_fun <- function(incr, cor_meth){
  for(i in c(seq(4, 9, incr))){
    g <- d_slide_all_complete %>%
      filter(age >= i, age < (i + incr)) %>%
      select(-character, -age) %>%
      cor(method = cor_meth) %>%
      ggcorrplot::ggcorrplot(hc.order = TRUE, 
                             title = paste0(i, " to ", i + incr, " years (n = ",
                                           data.frame(d_slide_all_complete %>%
                                                        filter(age >= i,
                                                               age < (i + incr)) %>%
                                                        count())$n,
                                           ")"))
    print(g)
  }
}
# make_corrplots_fun(incr = 0.5, cor_meth = "pearson")
# make_corrplots_fun(incr = 0.5, cor_meth = "spearman")
make_corrplots_fun(incr = 1.5, cor_meth = "spearman")

d_slide_all_complete %>%
  # filter(age >= i, age < (i + incr)) %>%
  select(-character, -age) %>%
  cor() %>%
  corrplot(method = "color", order = "hclust", tl.col = "black",
           title = "4-9 years (n = 250)")

d_slide_all_complete %>%
  filter(age >= 4, age < 7) %>%
  select(-character, -age) %>%
  cor() %>%
  corrplot(method = "color", order = "hclust", tl.col = "black",
           title = "4-6 years")

d_slide_all_complete %>%
  filter(age >= 7, age < 10) %>%
  select(-character, -age) %>%
  cor() %>%
  corrplot(method = "color", order = "hclust", tl.col = "black",
           title = "7-9 years")


# keep order constant ---

final_hclust <- d_slide_all_complete %>%
  mutate(age_rank = rank(age)) %>%
  top_n(120, age_rank) %>%
  select(-starts_with("age"), -character) %>%
  t() %>%
  dist() %>%
  hclust()

final_order <- data.frame(capacity = final_hclust$labels[final_hclust$order]) %>%
  rownames_to_column("order") %>%
  mutate(order = as.numeric(as.character(order)),
         capacity = gsub("\\.", " ", capacity))

d_slide_all_complete %>%
  mutate(age_rank = rank(age)) %>%
  filter(age_rank >= 1, age_rank < 121) %>%
  select(-starts_with("age"), -character) %>%
  cor() %>%
  data.frame() %>%
  rownames_to_column("cap1") %>%
  gather(cap2, cor, -cap1) %>%
  full_join(final_order, by = c("cap1" = "capacity")) %>%
  rename(order1 = order) %>%
  full_join(final_order, by = c("cap2" = "capacity")) %>%
  rename(order2 = order) %>%
  ggplot(aes(x = reorder(cap1, order1), 
             y = reorder(cap2, order2),
             fill = cor)) +
  geom_tile() +
  scale_fill_continuous(low = "white", high = "blue") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_blank())
