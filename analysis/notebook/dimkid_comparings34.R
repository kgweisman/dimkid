# run dimkid_cogsci_analysis first!

# comparing studies 3-4

# setup
temp <- d3 %>% select(-trial.comments) %>%
  full_join(d4) %>%
  distinct(age_group, testingSite, experimenter, subid, age) %>%
  mutate(testingSite = factor(tolower(as.character(testingSite)),
                              levels = c("bing", "jmz", "tech")),
         experimenter = factor(tolower(as.character(experimenter)),
                               levels = c("ay", "br", "cf", "cx", "jns",
                                          "kw", "ldb", "osh")))

# total n
temp %>% count(age_group)

# age
ggplot(temp, aes(x = age, fill = age_group)) +
  geom_histogram(bins = 50) +
  theme_bw() +
  scale_x_continuous(breaks = 0:12)

# gender - NEED INFO
# race/ethnicity - NEED INFO
# language - NEED INFO

# testing site
ggplot(temp, aes(x = age_group, fill = testingSite)) +
  geom_bar(position = "stack", color = 1, size = 0.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set3")

# experimenter
ggplot(temp, aes(x = age_group, fill = experimenter)) +
  geom_bar(position = "stack", color = 1, size = 0.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set3")


