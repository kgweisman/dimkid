d1_all_endorse %>%
  select(age_group, subid, character, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  count(age_group, character, 
        BODY, HEART) %>%
  complete(BODY, HEART, character, 
           age_group, fill = list(n = NA_integer_)) %>%
  group_by(age_group, character) %>%
  mutate(prop = round(n/sum(n, na.rm = T), 2)*100) %>%
  ungroup() %>%
  mutate(label = case_when(prop < 1 | is.na(prop) ~ "",
                           prop >= 1 ~ paste0(prop, "%"))) %>%
  ggplot(aes(x = BODY, y = HEART, fill = prop, label = label)) +
  facet_grid(character ~ age_group) +
  geom_tile(color = "black", show.legend = F) +
  geom_abline(lty = 2, color = "black") +
  geom_text(color = "black") +
  scale_fill_distiller(palette = "GnBu", direction = 1, 
                       # limits = c(0, 50),
                       na.value = "white") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank()) +
  labs(title = "Study 1: Patterns of endorsement of BODY and HEART items to target characters",
       subtitle = "Responses of either 'yes' or 'kinda' (but not 'no') were considered endorsements",
       x = "number of BODY items endorsed (out of 6)",
       y = "number of HEART items endorsed (out of 6)")

d1_all_endorse %>%
  select(age_group, subid, character, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  count(age_group, character, 
        BODY, MIND) %>%
  complete(BODY, MIND, character, 
           age_group, fill = list(n = NA_integer_)) %>%
  group_by(age_group, character) %>%
  mutate(prop = round(n/sum(n, na.rm = T), 2)*100) %>%
  ungroup() %>%
  mutate(label = case_when(prop < 1 | is.na(prop) ~ "",
                           prop >= 1 ~ paste0(prop, "%"))) %>%
  ggplot(aes(x = BODY, y = MIND, fill = prop, label = label)) +
  facet_grid(character ~ age_group) +
  geom_tile(color = "black", show.legend = F) +
  geom_abline(lty = 2, color = "black") +
  geom_text(color = "black") +
  scale_fill_distiller(palette = "GnBu", direction = 1, 
                       # limits = c(0, 50),
                       na.value = "white") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank()) +
  labs(title = "Study 1: Patterns of endorsement of BODY and MIND items to target characters",
       subtitle = "Responses of either 'yes' or 'kinda' (but not 'no') were considered endorsements",
       x = "number of BODY items endorsed (out of 6)",
       y = "number of MIND items endorsed (out of 6)")

d1_all_endorse %>%
  select(age_group, subid, character, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  count(age_group, character, 
        HEART, MIND) %>%
  complete(HEART, MIND, character, 
           age_group, fill = list(n = NA_integer_)) %>%
  group_by(age_group, character) %>%
  mutate(prop = round(n/sum(n, na.rm = T), 2)*100) %>%
  ungroup() %>%
  mutate(label = case_when(prop < 1 | is.na(prop) ~ "",
                           prop >= 1 ~ paste0(prop, "%"))) %>%
  ggplot(aes(x = HEART, y = MIND, fill = prop, label = label)) +
  facet_grid(character ~ age_group) +
  geom_tile(color = "black", show.legend = F) +
  geom_abline(lty = 2, color = "black") +
  geom_text(color = "black") +
  scale_fill_distiller(palette = "GnBu", direction = 1, 
                       # limits = c(0, 50),
                       na.value = "white") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank()) +
  labs(title = "Study 1: Patterns of endorsement of HEART and MIND items to target characters",
       subtitle = "Responses of either 'yes' or 'kinda' (but not 'no') were considered endorsements",
       x = "number of HEART items endorsed (out of 6)",
       y = "number of MIND items endorsed (out of 6)")



d2_all_endorse %>%
  mutate(sum_endorse = prop_endorse * n) %>%
  select(age_group, subid, anim_inan, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  count(age_group, anim_inan, 
        BODY, HEART) %>%
  complete(BODY, HEART, anim_inan, 
           age_group, fill = list(n = NA_integer_)) %>%
  group_by(age_group, anim_inan) %>%
  mutate(prop = round(n/sum(n, na.rm = T), 2)*100) %>%
  ungroup() %>%
  mutate(label = case_when(prop < 1 | is.na(prop) ~ "",
                           prop >= 1 ~ paste0(prop, "%"))) %>%
  ggplot(aes(x = BODY, y = HEART, fill = prop, label = label)) +
  facet_grid(anim_inan ~ age_group) +
  geom_tile(color = "black", show.legend = F) +
  geom_abline(lty = 2, color = "black") +
  geom_text(color = "black") +
  scale_fill_distiller(palette = "GnBu", direction = 1, 
                       # limits = c(0, 50),
                       na.value = "white") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank()) +
  labs(title = "Study 2: Patterns of endorsement of BODY and HEART items to target characters",
       subtitle = "Responses of either 'yes' or 'kinda' (but not 'no') were considered endorsements",
       x = "number of BODY items endorsed (out of 6)",
       y = "number of HEART items endorsed (out of 6)")

d2_all_endorse %>%
  mutate(sum_endorse = prop_endorse * n) %>%
  select(age_group, subid, anim_inan, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  count(age_group, anim_inan, 
        BODY, MIND) %>%
  complete(BODY, MIND, anim_inan, 
           age_group, fill = list(n = NA_integer_)) %>%
  group_by(age_group, anim_inan) %>%
  mutate(prop = round(n/sum(n, na.rm = T), 2)*100) %>%
  ungroup() %>%
  mutate(label = case_when(prop < 1 | is.na(prop) ~ "",
                           prop >= 1 ~ paste0(prop, "%"))) %>%
  ggplot(aes(x = BODY, y = MIND, fill = prop, label = label)) +
  facet_grid(anim_inan ~ age_group) +
  geom_tile(color = "black", show.legend = F) +
  geom_abline(lty = 2, color = "black") +
  geom_text(color = "black") +
  scale_fill_distiller(palette = "GnBu", direction = 1, 
                       # limits = c(0, 50),
                       na.value = "white") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank()) +
  labs(title = "Study 2: Patterns of endorsement of BODY and MIND items to target characters",
       subtitle = "Responses of either 'yes' or 'kinda' (but not 'no') were considered endorsements",
       x = "number of BODY items endorsed (out of 6)",
       y = "number of MIND items endorsed (out of 6)")

d2_all_endorse %>%
  mutate(sum_endorse = prop_endorse * n) %>%
  select(age_group, subid, anim_inan, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  count(age_group, anim_inan, 
        HEART, MIND) %>%
  complete(HEART, MIND, anim_inan, 
           age_group, fill = list(n = NA_integer_)) %>%
  group_by(age_group, anim_inan) %>%
  mutate(prop = round(n/sum(n, na.rm = T), 2)*100) %>%
  ungroup() %>%
  mutate(label = case_when(prop < 1 | is.na(prop) ~ "",
                           prop >= 1 ~ paste0(prop, "%"))) %>%
  ggplot(aes(x = HEART, y = MIND, fill = prop, label = label)) +
  facet_grid(anim_inan ~ age_group) +
  geom_tile(color = "black", show.legend = F) +
  geom_abline(lty = 2, color = "black") +
  geom_text(color = "black") +
  scale_fill_distiller(palette = "GnBu", direction = 1, 
                       # limits = c(0, 50),
                       na.value = "white") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank()) +
  labs(title = "Study 2: Patterns of endorsement of HEART and MIND items to target characters",
       subtitle = "Responses of either 'yes' or 'kinda' (but not 'no') were considered endorsements",
       x = "number of HEART items endorsed (out of 6)",
       y = "number of MIND items endorsed (out of 6)")




d3_all_endorse %>%
  filter(!is.na(age_group), !is.na(character)) %>%
  select(age_group, subid, character, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  count(age_group, character, 
        BODY, HEART) %>%
  complete(BODY, HEART, character, 
           age_group, fill = list(n = NA_integer_)) %>%
  group_by(age_group, character) %>%
  mutate(prop = round(n/sum(n, na.rm = T), 2)*100) %>%
  ungroup() %>%
  mutate(label = case_when(prop < 1 | is.na(prop) ~ "",
                           prop >= 1 ~ paste0(prop, "%"))) %>%
  ggplot(aes(x = BODY, y = HEART, fill = prop, label = label)) +
  facet_grid(character ~ age_group) +
  geom_tile(color = "black", show.legend = F) +
  geom_abline(lty = 2, color = "black") +
  geom_text(color = "black") +
  scale_fill_distiller(palette = "GnBu", direction = 1, 
                       # limits = c(0, 50),
                       na.value = "white") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank()) +
  labs(title = "Study 3: Patterns of endorsement of BODY and HEART items to target characters",
       subtitle = "Responses of either 'yes' or 'kinda' (but not 'no') were considered endorsements",
       x = "number of BODY items endorsed (out of 6)",
       y = "number of HEART items endorsed (out of 6)")

d3_all_endorse %>%
  filter(!is.na(age_group), !is.na(character)) %>%
  select(age_group, subid, character, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  count(age_group, character, 
        BODY, MIND) %>%
  complete(BODY, MIND, character, 
           age_group, fill = list(n = NA_integer_)) %>%
  group_by(age_group, character) %>%
  mutate(prop = round(n/sum(n, na.rm = T), 2)*100) %>%
  ungroup() %>%
  mutate(label = case_when(prop < 1 | is.na(prop) ~ "",
                           prop >= 1 ~ paste0(prop, "%"))) %>%
  ggplot(aes(x = BODY, y = MIND, fill = prop, label = label)) +
  facet_grid(character ~ age_group) +
  geom_tile(color = "black", show.legend = F) +
  geom_abline(lty = 2, color = "black") +
  geom_text(color = "black") +
  scale_fill_distiller(palette = "GnBu", direction = 1, 
                       # limits = c(0, 50),
                       na.value = "white") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank()) +
  labs(title = "Study 3: Patterns of endorsement of BODY and MIND items to target characters",
       subtitle = "Responses of either 'yes' or 'kinda' (but not 'no') were considered endorsements",
       x = "number of BODY items endorsed (out of 6)",
       y = "number of MIND items endorsed (out of 6)")

d3_all_endorse %>%
  filter(!is.na(age_group), !is.na(character)) %>%
  select(age_group, subid, character, factor, sum_endorse) %>%
  spread(factor, sum_endorse) %>%
  count(age_group, character, 
        HEART, MIND) %>%
  complete(HEART, MIND, character, 
           age_group, fill = list(n = NA_integer_)) %>%
  group_by(age_group, character) %>%
  mutate(prop = round(n/sum(n, na.rm = T), 2)*100) %>%
  ungroup() %>%
  mutate(label = case_when(prop < 1 | is.na(prop) ~ "",
                           prop >= 1 ~ paste0(prop, "%"))) %>%
  ggplot(aes(x = HEART, y = MIND, fill = prop, label = label)) +
  facet_grid(character ~ age_group) +
  geom_tile(color = "black", show.legend = F) +
  geom_abline(lty = 2, color = "black") +
  geom_text(color = "black") +
  scale_fill_distiller(palette = "GnBu", direction = 1, 
                       # limits = c(0, 50),
                       na.value = "white") +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank()) +
  labs(title = "Study 3: Patterns of endorsement of HEART and MIND items to target characters",
       subtitle = "Responses of either 'yes' or 'kinda' (but not 'no') were considered endorsements",
       x = "number of HEART items endorsed (out of 6)",
       y = "number of MIND items endorsed (out of 6)")
