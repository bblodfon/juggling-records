library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(lubridate)

# 2022-01-23 => data collection date
data = readr::read_csv(file = 'data/records_23_01_2022.csv')
# fixed manually some problems with \" and Date formatting so no problems now
readr::problems()

# Correct Logged == '--' cases
data = data %>%
  mutate(Logged = case_when(Logged == '--' ~ Date, TRUE ~ as.Date(Logged)))

# Most logged patterns
popular_patterns = data %>%
  group_by(`Trick id`) %>%
  summarise(num_jugglers = n_distinct(`User id`)) %>%
  arrange(`Trick id`)
  #arrange(desc(num_jugglers))

trick_id2name = data %>%
  group_by(`Trick id`) %>%
  distinct(`Trick Name`) %>%
  arrange(`Trick id`) %>%
  ungroup()

stopifnot(trick_id2name %>% pull(`Trick id`) == popular_patterns %>% pull(`Trick id`))

pop_pat = bind_cols(trick_id2name, tibble(num_jugglers = popular_patterns$num_jugglers))

# How many jugglers today have logged at least once a 5b cascade record?
data %>%
  filter(`Trick Name` == '5 ball cascade') %>%
  distinct(`User id`) %>%
  count()

set1_col = RColorBrewer::brewer.pal(n = 8, name = 'Set1')
my_cols  = c(rep(set1_col[1], 8), rep(set1_col[2], 12))

# Pattern Popularity Bar plot (20 patterns with more users)
pop_pat %>%
  arrange(desc(num_jugglers)) %>% slice(1:20) %>%
  mutate(`Trick Name` = forcats::fct_reorder(`Trick Name`, num_jugglers, .desc = TRUE)) %>% # properly arrange them
  ggplot(aes(x = `Trick Name`, y = num_jugglers, fill = `Trick Name`)) +
  geom_bar(stat = "identity", show.legend = F) +
  theme_classic() +
  scale_fill_manual(values = my_cols) + # override
  labs(title = 'Pattern Popularity Plot', y = "Number of Jugglers", x = "Pattern name") +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5))
ggsave(filename = 'img/pop.png', width = 7, height = 5, dpi = 600)

# The 7 most popular patterns
pop_pat_7       = pop_pat %>% arrange(desc(num_jugglers)) %>% slice(1:7)
pop_pat_7_ids   = pop_pat_7 %>% pull(`Trick id`)
pop_pat_7_names = pop_pat_7 %>% pull(`Trick Name`)

# Keep only the data for the 7 most popular patterns
data_pop = data %>% filter(`Trick id` %in% pop_pat_7_ids)

# Minimum date logged
min_date =
  data_pop %>%
  arrange(Logged) %>%
  slice(1) %>%
  pull(Logged)

# Maximum date logged
max_date =
  data_pop %>%
  arrange(desc(Logged)) %>%
  slice(1) %>%
  pull(Logged)

month_range   = seq(min_date, max_date, by = "month")
#quarter_range = seq(min_date, max_date, by = "quarter")
#year_range    = seq(min_date, max_date, by = "year")

num_logs = lapply(month_range, function(some_date) {
  data_pop %>%
    group_by(`Trick id`) %>%
    filter(Logged <= some_date, .preserve = T) %>%
    summarise(x = n_distinct(`User id`))
})

# check all trick ids are in the same order in the above result
for (i in 1:(length(num_logs)-1)) {
  stopifnot(num_logs[[i]][["Trick id"]] == num_logs[[i+1]][["Trick id"]])
}

# get the pattern names (to be used as column names)
ids = num_logs[[1]][["Trick id"]]
trick_names = trick_id2name %>%
  filter(`Trick id` %in% ids) %>%
  pull(`Trick Name`)
names(ids) = trick_names

# `trick_id` should be in the `num_logs[[i]]` tibble column `Trick id`
# for the following function to work as it should
get_num_jugglers_vec = function(num_logs, trick_id) {
  sapply(num_logs, function(tbl) {
    tbl %>%
      filter(`Trick id` == trick_id) %>%
      pull(x)
  })
}

data_tbl = tibble::tibble(
  date = month_range,
  '{names(ids)[1]}' := get_num_jugglers_vec(num_logs, trick_id = ids[1]),
  '{names(ids)[2]}' := get_num_jugglers_vec(num_logs, trick_id = ids[2]),
  '{names(ids)[3]}' := get_num_jugglers_vec(num_logs, trick_id = ids[3]),
  '{names(ids)[4]}' := get_num_jugglers_vec(num_logs, trick_id = ids[4]),
  '{names(ids)[5]}' := get_num_jugglers_vec(num_logs, trick_id = ids[5]),
  '{names(ids)[6]}' := get_num_jugglers_vec(num_logs, trick_id = ids[6]),
  '{names(ids)[7]}' := get_num_jugglers_vec(num_logs, trick_id = ids[7]))

data_tbl = data_tbl %>%
  tidyr::pivot_longer(cols = 2:8, names_to = 'Pattern', values_to = 'num_jugglers')

pat_colors = set1_col[1:7]
names(pat_colors) = pop_pat_7_names

data_tbl %>%
  ggplot(aes(x = date, y = num_jugglers, color = Pattern)) +
  geom_line() +
  scale_color_manual(values = pat_colors) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  ggtitle("Popularity across time") +
  xlab("Logged time") +
  ylab("Number of Jugglers") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = 'img/pop2.png', width = 7, height = 5, dpi = 600)

# Where does the bump occur?
num_logs[172] # after this!
num_logs[173]
num_logs[174]

month_range[172] # "2020-11-21"
month_range[173] # "2020-12-21"
month_range[174]
