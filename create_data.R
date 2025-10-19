#==============================================================================#
# Dataset Creation for Randomization App                                       #
#==============================================================================#

if (!requireNamespace('pak')) install.packages('pak')
pak::pkg_install(c('dplyr', 'tibble', 'stringr', 'tidyr', 'this.path'))
suppressMessages(library(dplyr)); library(stringr)
setwd(this.path::here())

set.seed(6)
df <- tibble::tibble(  # Create data
  id = 1:200,
  value_ed = round(runif(n = 200, min = 1, max = 10), 0),
  enjoy_reading = round(value_ed * .2 + runif(n = 200, min = 1, max = 8), 0),
  stress = round(runif(n = 200, min = 1, max = 10), 0),
  prob_read_at_home = value_ed + enjoy_reading * .8 - stress * .5 + 
    rnorm(n = 200, sd = 2),
  read_at_home_descr = ifelse(percent_rank(prob_read_at_home) >= .5, 1, 0),
  reading_score_descr = value_ed * 2 + enjoy_reading + stress * -.5 + 
    read_at_home_descr * 5 + rnorm(n = 200, mean = 20, sd = 3),
  subgroup = case_when(
    percent_rank(prob_read_at_home) >= .85 ~ 'always taker',
    percent_rank(prob_read_at_home) < .85 & 
      percent_rank(prob_read_at_home) > .25 ~ 'complier',
    percent_rank(prob_read_at_home) <= .25 ~ 'never taker'
  ),
  complier = ifelse(subgroup == 'complier', 1, 0),
  always_taker = ifelse(subgroup == 'always taker', 1, 0),
  never_taker = ifelse(subgroup == 'never taker', 1, 0),
  random_assignment = rbinom(n = 200, size = 1, prob = .5),
  read_at_home_rand = ifelse(
    random_assignment == 1 & subgroup == 'complier' | subgroup == 'always taker',
    1, 0
  ),
  reading_score_rand = value_ed * 3 + enjoy_reading * 1.5 + stress * -1 + 
    read_at_home_rand * 5 + rnorm(n = 200, mean = 10, sd = 3),
) |> 
  mutate(  # scale probabilities to fall between 0 and 1 & have mean of .5
    prob_read_at_home = round(
      (prob_read_at_home - min(prob_read_at_home)) / 
        (max(prob_read_at_home) - min(prob_read_at_home)) * .85,
      digits = 3
    ),
    reading_score_descr = round(reading_score_descr, digits = 1),
    reading_score_rand = round(reading_score_rand, digits = 1)
  )
arrow::write_parquet(df, 'data/data.parquet')

df_mean <- df |>  # Convert to means for use in app
  rename_with(~ str_replace(.x, '_descr', '.descr')) |> 
  rename_with(~ str_replace(.x, '_rand', '.rand')) |> 
  rename(random_assignment.rand = random_assignment) |> 
  tidyr::pivot_longer(
    matches('\\.(descr|rand)'),
    names_to = c('.value', 'scenario'),
    names_sep = '\\.'
  ) |> 
  mutate(
    treat_var = ifelse(scenario == 'descr', read_at_home, random_assignment),
    treat_var = ifelse(treat_var == 1, 'Yes', 'No')
  ) |> 
  summarise(
    across(matches('value|enjoy|stress|reading'), ~ round(mean(.x), 1)),
    across(matches('complier|always|never|read_at'), ~ round(mean(.x), 3)),
    .by = c(scenario, treat_var)
  ) |> 
  arrange(scenario, treat_var)
arrow::write_parquet(df_mean, 'data/data_mean.parquet')

df_mean_cace <- df |>  # For CACE omniscient table
  mutate(treat_var = ifelse(random_assignment == 1, 'Yes', 'No')) |> 
  summarise(
    reading_score = mean(reading_score_rand),
    .by = c(subgroup, treat_var)
  ) |> 
  filter(subgroup == 'complier') %>%
  bind_rows(
    filter(df_mean, scenario == 'rand') |> 
    select(treat_var, reading_score) |> 
      mutate(subgroup = 'all'), 
    .
  ) |> 
  tidyr::pivot_wider(names_from = treat_var, values_from = reading_score) |> 
  mutate(
    var = ifelse(
      subgroup == 'all', "Reading Score,<br>All", "Reading Score,<br>Compliers"
    )
  ) |> 
  relocate(var) |> 
  select(-subgroup)
arrow::write_parquet(df_mean_cace, 'data/data_mean_cace.parquet')

df_mean_long <- df_mean |> 
  mutate(
    d_prob_read_at_home = prob_read_at_home * 10,
    e_complier = complier * 10,
    f_always = always_taker * 10,
    g_never = never_taker * 10
  ) |> 
  rename(
    a_stress = stress, b_value_ed = value_ed, c_enjoy_reading = enjoy_reading
  ) %>%
  tidyr::pivot_longer(
    3:ncol(.),
    names_to = "var",
    values_to = "value"
  ) |> 
  mutate(
    prob = case_when(
      var == 'd_prob_read_at_home' ~ value[var == 'prob_read_at_home'],
      var == 'e_complier' ~ value[var == 'complier'],
      var == 'f_always' ~ value[var == 'always_taker'],
      var == 'g_never' ~ value[var == 'never_taker']
    ),
    .by = c(scenario, treat_var)
  )
arrow::write_parquet(df_mean_long, 'data/data_mean_long.parquet')

df_mean_long_cace <- df |>  # For CACE omniscient plot
  filter(subgroup == 'complier') |> 
  rename_with(~ str_replace(.x, '_descr', '.descr')) |> 
  rename_with(~ str_replace(.x, '_rand', '.rand')) |> 
  rename(random_assignment.rand = random_assignment) |> 
  tidyr::pivot_longer(
    matches('\\.(descr|rand)'),
    names_to = c('.value', 'scenario'),
    names_sep = '\\.'
  ) |> 
  mutate(
    treat_var = ifelse(scenario == 'descr', read_at_home, random_assignment),
    treat_var = ifelse(treat_var == 1, 'Yes', 'No')
  ) |> 
  summarise(
    across(matches('value|enjoy|stress|reading'), ~ round(mean(.x), 1)),
    across(matches('complier|always|never|read_at'), ~ round(mean(.x), 3)),
    .by = c(scenario, treat_var)
  ) |> 
  arrange(scenario, treat_var) |> 
  mutate(
    d_prob_read_at_home = prob_read_at_home * 10,
    e_complier = complier * 10,
    f_always = always_taker * 10,
    g_never = never_taker * 10
  ) |> 
  rename(
    a_stress = stress, b_value_ed = value_ed, c_enjoy_reading = enjoy_reading
  ) %>%
  tidyr::pivot_longer(
    3:ncol(.),
    names_to = "var",
    values_to = "value"
  ) |> 
  mutate(
    prob = case_when(
      var == 'd_prob_read_at_home' ~ value[var == 'prob_read_at_home'],
      var == 'e_complier' ~ value[var == 'complier'],
      var == 'f_always' ~ value[var == 'always_taker'],
      var == 'g_never' ~ value[var == 'never_taker']
    ),
    .by = c(scenario, treat_var)
  ) |> 
  filter(scenario == 'rand') |> 
  mutate(
    value = ifelse(
      grepl('^(e|f|g)_', var) & treat_var == 'No', 
      df_mean_long$value[
        df_mean_long$treat_var == 'No' & df_mean_long$scenario == 'rand'
      ][
        match(var, df_mean_long$var[df_mean_long$treat_var == 'No'])
      ],
      value
    ),
    value = ifelse(
      grepl('^(e|f|g)_', var) & treat_var == 'Yes', 
      df_mean_long$value[
        df_mean_long$treat_var == 'Yes' & df_mean_long$scenario == 'rand'
      ][
        match(var, df_mean_long$var[df_mean_long$treat_var == 'Yes'])
      ],
      value
    ),
    prob = ifelse(
      grepl('^(e|f|g)_', var) & treat_var == 'No', 
      df_mean_long$prob[
        df_mean_long$treat_var == 'No' & df_mean_long$scenario == 'rand'
      ][
        match(var, df_mean_long$var[df_mean_long$treat_var == 'No'])
      ],
      prob
    ),
    prob = ifelse(
      grepl('^(e|f|g)_', var) & treat_var == 'Yes', 
      df_mean_long$prob[
        df_mean_long$treat_var == 'Yes' & df_mean_long$scenario == 'rand'
      ][
        match(var, df_mean_long$var[df_mean_long$treat_var == 'Yes'])
      ],
      prob
    )
  )
arrow::write_parquet(df_mean_long_cace, 'data/data_mean_long_cace.parquet')


