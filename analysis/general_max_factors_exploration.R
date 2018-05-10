max_fact_fun <- function(p) {
  
  s_moments <- function(p) {p*(p+1)/2}
  param_est <- function(p, k) {p*k + p - (k*(k-1)/2)}
  check_ok <- function(p, k) {
    a <- (p-k)^2
    b <- p+k
    return(ifelse(a>b, TRUE, FALSE))
  }
  
  df_check <- data.frame()
  for(i in 1:p){
    df_check[i,"check"] <- check_ok(p,i)
  }
  
  max <- df_check %>% filter(check) %>% nrow()
  return(max)
  
}

d_temp <- data.frame(p = 1:60) %>%
  group_by(p) %>%
  mutate(max = max_fact_fun(p))

d_temp %>%
  ggplot(aes(x = p, y = max)) +
  geom_abline(slope = 1, intercept = -1, lty = 2) +
  annotate("text", x = 12, y = 20, hjust = 0.5, size = 3,
           label = "misconception #1:\nmax k = p - 1") +
  geom_abline(slope = 1/3, intercept = 0, lty = 2) +
  annotate("text", x = 24, y = 2, hjust = 0.5, size = 3,
           label = "KW's mistake #2:\nmax k = p/3") +
  geom_abline(slope = 3/4, intercept = 0, lty = 2, color = "dodgerblue") +
  annotate("text", x = 48, y = 24, hjust = 0.5, size = 3, color = "dodgerblue",
           label = "new rule of thumb for\n10 < p < 50:\nmax k ~ 0.75p") +
  geom_abline(slope = 1/3, intercept = 0, lty = 2) +
  geom_path(color = "blue", size = 1) +
  annotate("segment", x = 20, xend = 20, y = max_fact_fun(20) + 0.75, 
           yend = max_fact_fun(20) + 3, color = "red", size = 1,
           arrow = arrow(length = unit(0.1, "inches"), ends = "first")) +
  annotate("segment", x = 40, xend = 40, y = max_fact_fun(40) + 0.75, 
           yend = max_fact_fun(40) + 3, color = "red", size = 1,
           arrow = arrow(length = unit(0.1, "inches"), ends = "first")) +
  scale_x_continuous(breaks = seq(0, 100, 4)) +
  scale_y_continuous(breaks = seq(0, 100, 4)) +
  theme_minimal() +
  labs(title = "maximal model size for datasets of different sizes",
       subtitle = "degrees of freedom constraint: (p - k)^2 > p + k",
       x = "number of variables (p)",
       y = "maximum number of factors to estimate (k)") +
  coord_equal()

summary(lm(max ~ p, data = d_temp))

        