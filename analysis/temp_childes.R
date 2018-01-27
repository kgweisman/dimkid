# devtools::install_github("langcog/childesr")
library(childesr)
library(tidyverse)

# get struff from CHILDES
# transcripts <- get_transcripts(collection = "Eng-NA") #, corpus = "Brown")

# utterances <- get_utterances(collection = "Eng-NA",
#                              # corpus = "Brown",
#                              role = c("target_child", "adult"))

tokens <- get_tokens(collection = "Eng-NA",
                     # corpus = "Brown",
                     role = c("target_child", "adult"),
                     age = NULL, sex = NULL, # child = "Adam",
                     token = c("happy", "sad", "scared", "angry",
                               "temperature", "smell", "hungry",
                               "tired", "pain", "sick", "choice",
                               "figure", "remember", "love",
                               "guilty", "hurt feelings", "embarrassed",
                               "aware", "proud"))

types <- get_types(collection = "Eng-NA",
                   # corpus = "Brown",
                   role = c("target_child", "adult"),
                   age = NULL,
                   sex = NULL,
                   # child = "Adam",
                   type = c("happy", "sad", "scared", "angry",
                            "temperature", "smell", "hungry",
                            "tired", "pain", "sick", "choice",
                            "figure", "remember", "love",
                            "guilty", "hurt feelings", "embarrassed",
                            "aware", "proud"))

speaker_stats <- get_speaker_statistics(collection = "Eng-NA",
                                        # corpus = "Brown",
                                        role = c("target_child", "adult"),
                                        age = NULL, sex = NULL) # child = "Adam")

# make types df and wordlist

tokens_df <- tokens %>%
  mutate(gloss = tolower(gloss)) %>%
  filter(!is.na(gloss), !gloss %in% c("", "lift", "spell", "stick")) %>%
  mutate(factor = recode(gloss,
                         angry = "BODY-HEART",
                         aware = "MIND",
                         choice = "MIND",
                         embarrassed = "HEART",
                         figure = "MIND",
                         guilty = "HEART",
                         happy = "BODY-HEART",
                         hungry = "BODY",
                         love = "HEART",
                         pain = "BODY",
                         proud = "HEART",
                         remember = "MIND",
                         sad = "HEART",
                         scared = "BODY",
                         sick = "BODY",
                         smell = "BODY",
                         temperature = "MIND",
                         tired = "BODY")) %>%
  full_join(speaker_stats %>% 
              ungroup() %>%
              select(corpus_id, transcript_id, starts_with("num"), mlu) %>%
              distinct())

types_df <- types %>%
  mutate(gloss = tolower(gloss)) %>%
  filter(!is.na(gloss), !gloss %in% c("", "lift", "spell", "stick")) %>%
  mutate(factor = recode(gloss,
                         angry = "BODY-HEART",
                         aware = "MIND",
                         choice = "MIND",
                         embarrassed = "HEART",
                         figure = "MIND",
                         guilty = "HEART",
                         happy = "BODY-HEART",
                         hungry = "BODY",
                         love = "HEART",
                         pain = "BODY",
                         proud = "HEART",
                         remember = "MIND",
                         sad = "HEART",
                         scared = "BODY",
                         sick = "BODY",
                         smell = "BODY",
                         temperature = "MIND",
                         tired = "BODY")) %>%
  full_join(speaker_stats %>% 
              ungroup() %>%
              select(corpus_id, transcript_id, starts_with("num"), mlu) %>%
              distinct())

word_list <- types_df %>%
  distinct(factor, gloss) %>%
  group_by(factor) %>%
  mutate(list = paste(gloss, collapse = ", ")) %>%
  ungroup() %>%
  distinct(factor, list) %>%
  filter(!is.na(factor))

tokens_heard_df <- tokens_df %>%
  filter(speaker_role == "Adult", 
         target_child_age/365.25 < 10)

tokens_heard_df_med_ages <- tokens_heard_df %>%
  filter(target_child_age/365.25 < 10) %>%
  group_by(factor) %>%
  summarise(median = median(target_child_age)/365.25)

types_heard_df <- types_df %>%
  filter(speaker_role == "Adult", 
         target_child_age/365.25 < 10) %>%
  mutate(prop = count/num_types) %>%
  filter(!is.na(prop))

tokens_prod_df <- tokens_df %>%
  filter(speaker_role == "Adult", 
         target_child_age/365.25 < 10)

tokens_prod_df_med_ages <- tokens_prod_df %>%
  filter(target_child_age/365.25 < 10) %>%
  group_by(factor) %>%
  summarise(median = median(target_child_age)/365.25)

types_prod_df <- types_df %>%
  filter(speaker_role == "Target_Child",
         target_child_age/365.25 < 10) %>%
  mutate(prop = count/num_types) %>%
  filter(!is.na(prop))

ggplot(tokens_heard_df %>%
         filter(target_child_age/365.25 < 10),
       aes(x = target_child_age/365.25,
           fill = factor)) +
  facet_wrap(~ factor) +
  geom_histogram(alpha = 0.5, position = position_identity(),
                 binwidth = .5) +
  geom_vline(data = tokens_heard_df_med_ages,
             aes(xintercept = median), lty = 2) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10000, 5)) +
  scale_fill_manual(values = c("#e41a1c", "#984ea3", "#377eb8", "#4daf4a")) +
  labs(title = "Overheard adult speech over age",
       # subtitle = "Brown corpus (CHILDES)",
       subtitle = "All corpora, all target children ≤10 years (CHILDES). Dotted line is the median.",
       x = "Age (years)",
       y = "Count",
       fill = "Factor\n(from 7-9yo EFA)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  geom_text(data = word_list,
            aes(label = list),
            x = 0,
            y = 22,
            hjust = 0, size = 3)

ggplot(tokens_prod_df %>%
         filter(target_child_age/365.25 < 10),
       aes(x = target_child_age/365.25,
           fill = factor)) +
  facet_wrap(~ factor) +
  geom_histogram(alpha = 0.5, position = position_identity(),
                 binwidth = .5) +
  geom_vline(data = tokens_prod_df_med_ages,
             aes(xintercept = median), lty = 2) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10000, 50)) +
  scale_fill_manual(values = c("#e41a1c", "#984ea3", "#377eb8", "#4daf4a")) +
  labs(title = "Production over age",
       subtitle = "All corpora, all target children ≤10 years (CHILDES). Dotted line is the median.",
       x = "Age (years)",
       y = "Count",
       fill = "Factor\n(from 7-9yo EFA)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  geom_text(data = word_list,
            aes(label = list),
            x = 0,
            y = 375,
            hjust = 0, size = 3)

ggplot(types_heard_df,
       aes(x = target_child_age/365.25,
           y = prop,
           color = factor)) +
  facet_wrap(~ factor) +
  # geom_smooth(method = "loess") +
  geom_point(alpha = 0.3) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_color_manual(values = c("#e41a1c", "#984ea3", "#377eb8", "#4daf4a")) +
  labs(title = "Overheard adult speech over age",
       # subtitle = "Brown corpus (CHILDES)",
       subtitle = "All corpora, all target children ≤10 years (CHILDES)",
       x = "Age (years)",
       y = "Proportion of types in transcript",
       fill = "Factor\n(from 7-9yo EFA)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  geom_text(data = word_list,
            aes(label = list),
            x = 0, y = 1, color = "black",
            hjust = 0, size = 3)

ggplot(types_prod_df,
       aes(x = target_child_age/365.25,
           y = prop,
           color = factor,
           fill = factor)) +
  facet_wrap(~ factor) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "black") +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_fill_manual(values = c("#e41a1c", "#984ea3", "#377eb8", "#4daf4a")) +
  scale_color_manual(values = c("#e41a1c", "#984ea3", "#377eb8", "#4daf4a")) +
  labs(title = "Production over age",
       # subtitle = "Brown corpus (CHILDES)",
       subtitle = "All corpora, all target children ≤10 years (CHILDES)",
       x = "Age (years)",
       y = "Proportion of types in transcript",
       fill = "Factor\n(from 7-9yo EFA)",
       color = "Factor\n(from 7-9yo EFA)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  geom_text(data = word_list,
            aes(label = list),
            x = 0, y = 1, color = "black",
            hjust = 0, size = 3)
