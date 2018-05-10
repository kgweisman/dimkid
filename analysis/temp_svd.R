# SETUP -----

# FOR OLDER KIDS
# first run cogsci2018_dimkid.Rmd
# get means by character by capacity for old cohort
d_79_mean <- d_old %>%
  group_by(character, capacity) %>%
  summarise(mean = mean(responseNum)) %>%
  ungroup() %>%
  spread(character, mean) %>%
  remove_rownames() %>%
  data.frame() %>%
  column_to_rownames("capacity")

# FOR ADULTS (PNAS STUDY 1)
d_adult_mean <- read.csv("https://osf.io/kdzge/download") %>%
  select(subid, condition, happy:pride) %>%
  gather(capacity, response, -subid, -condition) %>%
  group_by(condition, capacity) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  ungroup() %>%
  spread(condition, mean) %>%
  remove_rownames() %>%
  data.frame() %>%
  column_to_rownames("capacity")

# set base
d_base_mean <- d_79_mean
# d_base_mean <- d_adult_mean

# get item and property names
item_names <- names(d_base_mean)
property_names <- rownames(d_base_mean)

# do efa
efa_factors <- psych::fa(t(d_base_mean), 3, rotate = "varimax")$loadings[] %>%
  data.frame() %>%
  rownames_to_column("property") %>%
  gather(factor, loading, -property) %>%
  group_by(property) %>%
  top_n(1, abs(loading)) %>%
  mutate(factor = factor(factor, 
                         levels = c("MR1", "MR2", "MR3"),
                         labels = c("BODY", "HEART", "MIND")))

# SVD -----
# do svd for old cohort (get 4 modes)
svd_base <- svd(d_base_mean, 4, 4)
u_old <- svd_base$u
s_old <- svd_base$d
v_old <- svd_base$v

# visualize feature vectors (u)
ggplot(data.frame(u_old) %>%
         mutate(X1 = X1 * s_old[1],
                X2 = X2 * s_old[2],
                X3 = X3 * s_old[3],
                X4 = X4 * s_old[4]) %>%
         rownames_to_column("property") %>%
         arrange(desc(abs(X1)), desc(abs(X2)), 
                 desc(abs(X3)), desc(abs(X4))) %>%
         mutate(order = 1:nrow(d_base_mean)) %>%
         gather(mode, value, -property, -order) %>%
         mutate(mode = gsub("X", "", mode)) %>%
         mutate_at(vars(property, mode), funs(as.numeric)) %>%
         mutate(mode = factor(mode, labels = c("mode_1", "mode_2", 
                                               "mode_3", "mode_4")),
                property = factor(property, labels = property_names)),
       aes(x = mode, y = reorder(property, desc(order)),
           fill = value)) + 
  geom_tile(color = "black") +
  geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  theme_minimal() +
  labs(title = "U * S: Feature vectors multiplied by singular value (by mode)",
       x = "mode",
       y = "property")

# alternative visualization of feature vectors (u)
data.frame(u_old) %>%
  mutate(X1 = X1 * s_old[1],
         X2 = X2 * s_old[2],
         X3 = X3 * s_old[3],
         X4 = X4 * s_old[4]) %>%
  rownames_to_column("property") %>%
  arrange(desc(abs(X1)), desc(abs(X2)), 
          desc(abs(X3)), desc(abs(X4))) %>%
  mutate(order = 1:nrow(d_base_mean)) %>%
  gather(mode, value, -property, -order) %>%
  mutate(mode = gsub("X", "", mode)) %>%
  mutate_at(vars(property, mode), funs(as.numeric)) %>%
  mutate(mode = factor(mode, labels = c("mode_1", "mode_2", 
                                        "mode_3", "mode_4")),
         property = factor(property, labels = property_names)) %>%
  full_join(efa_factors %>% select(property, factor)) %>%
  ggplot(aes(x = mode, y = value, label = property, color = factor)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point() +
  ggrepel::geom_text_repel(size = 3, segment.size = 0.1) + #, direction = "x") +
  # geom_text(size = 2, position = position_nudge(x = 0.06), hjust = 0) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "U * S: Feature vectors multiplied by singular value (by mode)",
       x = "mode",
       y = "U-vector value * singular value")

# visualize classifier vectors (v)
ggplot(data.frame(v_old) %>%
         mutate(X1 = X1 * s_old[1],
                X2 = X2 * s_old[2],
                X3 = X3 * s_old[3],
                X4 = X4 * s_old[4]) %>%
         rownames_to_column("item") %>%
         arrange(desc(abs(X1)), desc(abs(X2)), 
                 desc(abs(X3)), desc(abs(X4))) %>%
         mutate(order = 1:ncol(d_base_mean)) %>%
         gather(mode, value, -item, -order) %>%
         mutate(mode = gsub("X", "", mode)) %>%
         mutate_at(vars(item, mode), funs(as.numeric)) %>%
         mutate(mode = factor(mode, labels = c("mode_1", "mode_2", 
                                               "mode_3", "mode_4")),
                item = factor(item, labels = item_names)),
       aes(x = mode, y = reorder(item, desc(order)),
           fill = value)) + 
  geom_tile(color = "black") +
  geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  theme_minimal() +
  labs(title = "V * S: Classification vectors multiplied by singular value (by mode)",
       x = "mode",
       y = "item")

# OUTER PRODUCTS * SINGULAR VALUE -----
# calculate outer products, multiply by singular value
m_d1 <- outer(u_old[,1], v_old[,1]) * s_old[1]
m_d2 <- outer(u_old[,2], v_old[,2]) * s_old[2]
m_d3 <- outer(u_old[,3], v_old[,3]) * s_old[3]
m_d4 <- outer(u_old[,4], v_old[,4]) * s_old[4]

ggplot(data.frame(m_d1) %>%
         rownames_to_column("property") %>%
         gather(item, value, -property) %>%
         mutate(item = gsub("X", "", item)) %>%
         mutate_at(vars(property, item), funs(as.numeric)) %>%
         mutate(item = factor(item, labels = item_names),
                property = factor(property, labels = property_names)),
       aes(#x = item, y = property,
           x = reorder(item, value), y = reorder(property, value), 
           fill = value)) + 
  geom_tile(color = "black") +
  geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
  scale_fill_distiller(type = "div", 
                       limits = c(-1, 1),
                       palette = "PRGn") +
  theme_minimal() +
  labs(title = "Mode 1: outer product * singular value matrix",
       x = "item",
       y = "property")

ggplot(data.frame(m_d2) %>%
         rownames_to_column("property") %>%
         gather(item, value, -property) %>%
         mutate(item = gsub("X", "", item)) %>%
         mutate_at(vars(property, item), funs(as.numeric)) %>%
         mutate(item = factor(item, labels = item_names),
                property = factor(property, labels = property_names)),
       aes(#x = item, y = property,
         x = reorder(item, value), y = reorder(property, value), 
         fill = value)) + 
  geom_tile(color = "black") +
  geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
  scale_fill_distiller(type = "div", 
                       limits = c(-1, 1),
                       palette = "PRGn") +
  theme_minimal() +
  labs(title = "Mode 2: outer product * singular value matrix",
       x = "item",
       y = "property")

ggplot(data.frame(m_d3) %>%
         rownames_to_column("property") %>%
         gather(item, value, -property) %>%
         mutate(item = gsub("X", "", item)) %>%
         mutate_at(vars(property, item), funs(as.numeric)) %>%
         mutate(item = factor(item, labels = item_names),
                property = factor(property, labels = property_names)),
       aes(#x = item, y = property,
         x = reorder(item, value), y = reorder(property, value), 
         fill = value)) + 
  geom_tile(color = "black") +
  geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
  scale_fill_distiller(type = "div", 
                       limits = c(-1, 1),
                       palette = "PRGn") +
  theme_minimal() +
  labs(title = "Mode 3: outer product * singular value matrix",
       x = "item",
       y = "property")

ggplot(data.frame(m_d4) %>%
         rownames_to_column("property") %>%
         gather(item, value, -property) %>%
         mutate(item = gsub("X", "", item)) %>%
         mutate_at(vars(property, item), funs(as.numeric)) %>%
         mutate(item = factor(item, labels = item_names),
                property = factor(property, labels = property_names)),
       aes(#x = item, y = property,
         x = reorder(item, value), y = reorder(property, value), 
         fill = value)) + 
  geom_tile(color = "black") +
  geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
  scale_fill_distiller(type = "div", 
                       limits = c(-1, 1),
                       palette = "PRGn") +
  theme_minimal() +
  labs(title = "Mode 4: outer product * singular value matrix",
       x = "item",
       y = "property")

# REGRESSION -----

# turn everything into vectors
# data
d_base_vec <- d_base_mean %>% 
  rownames_to_column("property") %>%
  gather(item, mean, -property) %>%
  mutate(item = factor(item, labels = item_names),
         property = factor(property, labels = property_names)) %>%
  arrange(item, property) %>%
  select(mean)

# mode_1
mode_1_vec <- outer(u_old[,1], v_old[,1]) %>%
  data.frame() %>%
  rownames_to_column("property") %>%
  gather(item, value, -property) %>%
  mutate(item = gsub("X", "", item)) %>%
  mutate_at(vars(property, item), funs(as.numeric)) %>%
  mutate(item = factor(item, labels = item_names),
         property = factor(property, labels = property_names)) %>%
  arrange(item, property) %>%
  select(value) %>%
  rename(mode_1 = value)

# mode_2
mode_2_vec <- outer(u_old[,2], v_old[,2]) %>%
  data.frame() %>%
  rownames_to_column("property") %>%
  gather(item, value, -property) %>%
  mutate(item = gsub("X", "", item)) %>%
  mutate_at(vars(property, item), funs(as.numeric)) %>%
  mutate(item = factor(item, labels = item_names),
         property = factor(property, labels = property_names)) %>%
  arrange(item, property) %>%
  select(value) %>%
  rename(mode_2 = value)

# mode_3
mode_3_vec <- outer(u_old[,3], v_old[,3]) %>%
  data.frame() %>%
  rownames_to_column("property") %>%
  gather(item, value, -property) %>%
  mutate(item = gsub("X", "", item)) %>%
  mutate_at(vars(property, item), funs(as.numeric)) %>%
  mutate(item = factor(item, labels = item_names),
         property = factor(property, labels = property_names)) %>%
  arrange(item, property) %>%
  select(value) %>%
  rename(mode_3 = value)

# mode_4
mode_4_vec <- outer(u_old[,4], v_old[,4]) %>%
  data.frame() %>%
  rownames_to_column("property") %>%
  gather(item, value, -property) %>%
  mutate(item = gsub("X", "", item)) %>%
  mutate_at(vars(property, item), funs(as.numeric)) %>%
  mutate(item = factor(item, labels = item_names),
         property = factor(property, labels = property_names)) %>%
  arrange(item, property) %>%
  select(value) %>%
  rename(mode_4 = value)

# combine
d_reg <- cbind(d_base_vec, mode_1_vec, mode_2_vec, mode_3_vec, mode_4_vec)

# regression
r1 <- lm(mean ~ mode_1, d_reg); summary(r1)
r2 <- lm(mean ~ mode_2, d_reg); summary(r2)
r3 <- lm(mean ~ mode_3, d_reg); summary(r3)
r4 <- lm(mean ~ mode_4, d_reg); summary(r4)
r12 <- lm(mean ~ mode_1 + mode_2, d_reg); summary(r12)
r123 <- lm(mean ~ mode_1 + mode_2 + mode_3, d_reg); summary(r123)
r1234 <- lm(mean ~ mode_1 + mode_2 + mode_3 + mode_4, d_reg); summary(r1234)

anova(r1, r12, r123, r1234)

summary(r1234)$coefficients %>%
  data.frame() %>%
  rownames_to_column("parameter") %>%
  mutate(SV = c(NA, s_old[1:4])) %>%
  rename(beta = Estimate,
         beta_se = Std..Error,
         beta_t = t.value,
         beta_p = Pr...t..) %>%
  filter(grepl("mode", parameter))
