# 2018-05-08
# check for differences in data used in reanalysis vs. cogsci paper

# checking 7-9yo data -----

# get new dataset from dimkid_reanalysis.Rmd
temp_new <- d_us79_9char %>% 
  rownames_to_column("subid") %>% 
  gather(mc, response, -subid) %>%
  arrange(subid, mc) %>%
  rename(response_new = response)

# get old dataset from cogsci2018_dimkid.Rmd
temp_old <- d_old_wide %>% 
  rownames_to_column("subid") %>% 
  gather(mc, response, -subid) %>%
  arrange(subid, mc) %>%
  rename(response_old = response)

# join them and compare response by subid and capacity
temp_comp <- temp_new %>%
  full_join(temp_old) %>%
  mutate(match = (response_new == response_old))

# how many mismatches? should be 0
temp_comp %>% 
  filter(!(is.na(response_old) & is.na(response_new)), is.na(match) | !match) %>%
  nrow()

# same number of rows? should be TRUE
nrow(temp_new) == nrow(temp_old)

# same number of distinct subids? should be TRUE
temp_new %>% distinct(subid) %>% count() %>% as.numeric() == temp_old %>% distinct(subid) %>% count() %>% as.numeric()

# same number of distinct mcs? should be TRUE
temp_new %>% distinct(mc) %>% count() %>% as.numeric() == temp_old %>% distinct(mc) %>% count() %>% as.numeric()


# checking 4-6yo data -----

# get new dataset from dimkid_reanalysis.Rmd
temp_new <- d_us46_9char %>% 
  rownames_to_column("subid") %>% 
  gather(mc, response, -subid) %>%
  arrange(subid, mc) %>%
  rename(response_new = response)

# get old dataset from cogsci2018_dimkid.Rmd
temp_old <- d_young_wide %>% 
  rownames_to_column("subid") %>% 
  gather(mc, response, -subid) %>%
  arrange(subid, mc) %>%
  rename(response_old = response)

# join them and compare response by subid and capacity
temp_comp <- temp_new %>%
  full_join(temp_old) %>%
  mutate(match = (response_new == response_old))

# how many mismatches? should be 0
temp_comp %>% 
  filter(!(is.na(response_old) & is.na(response_new)), is.na(match) | !match) %>%
  nrow()

# same number of rows? should be TRUE
nrow(temp_new) == nrow(temp_old)

# same number of distinct subids? should be TRUE
temp_new %>% distinct(subid) %>% count() %>% as.numeric() == temp_old %>% distinct(subid) %>% count() %>% as.numeric()

# same number of distinct mcs? should be TRUE
temp_new %>% distinct(mc) %>% count() %>% as.numeric() == temp_old %>% distinct(mc) %>% count() %>% as.numeric()

# findings -----
# no differences found