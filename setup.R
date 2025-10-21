#==============================================================================#
# Data & Functions for Randomization App                                       #
#==============================================================================#

library(dplyr); library(stringr)
library(ggplot2); library(DiagrammeR); library(gt)
df <- arrow::read_parquet('data/data_mean.parquet')
df_cace <- arrow::read_parquet('data/data_mean_cace.parquet')
df_long <- arrow::read_parquet('data/data_mean_long.parquet')
df_long_cace <- arrow::read_parquet('data/data_mean_long_cace.parquet')
df_full <- arrow::read_parquet('data/data.parquet')

# Plots ========================================================================

## ATE -------------------------------------------------------------------------

make_plot_ate <- function(df) {
  plt <- ggplot(df, aes(treat_var, reading_score, fill = treat_var)) +
    geom_col() + 
    geom_text(
      aes(label = reading_score), vjust = 2, 
      family = 'Roboto', color = "white", size = 7
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      legend.position = 'none',
      text = element_text(family = "Roboto", size = 18),
      plot.title = element_text(size = 18, hjust = .5),
      axis.text.y = element_blank()
    ) +
    scale_fill_manual(
      values = c('Yes' = 'deepskyblue2', 'No' = 'deeppink2')
    ) +
    ylab('Reading Score')
  if ('descr' %in% df$scenario) {
    plt + 
      xlab('Read-At-Home, Participated') +
      ggtitle('Average Reading Scores by\nParticipation in Read-At-Home')
  } else {
    plt + 
      xlab('Read-At-Home, Assigned') +
      ggtitle('Average Reading Scores by\nAssignment to Read-At-Home')
  }
}

## Balance ---------------------------------------------------------------------

make_plot_bal <- function(df) {
  if ('omni' %in% names(df)) {  # Actual plots for omniscient verison
    if ('strata' %in% names(df)) {  # Filter vars to plot & create their labels
      df <- df |>
        filter(
          var %in% c(
            "a_stress", "b_value_ed", "c_enjoy_reading", "d_prob_read_at_home",
            "e_complier", "f_always", "g_never"
          )
        )
      x_lab <- c(
        "Stress", "Value Edu", "Enjoy Read", "Prob\nParticip. RAH",
        "Complier", "Always Taker", "Never Taker"
      )
    } else {
      df <- df |>
        filter(
          var %in% c(
            "a_stress", "b_value_ed", "c_enjoy_reading", "d_prob_read_at_home"
          )
        )
      x_lab <- c("Stress", "Value Edu", "Enjoy Read", "Prob\nParticip. RAH")
    }
    
    if ('descr' %in% df$scenario) {  # Legend labels
      leg_lab <- 'Particip.\nin RAH'
    } else {
      leg_lab <- 'Assign\nto RAH'
    }
    if ('strata' %in% names(df)) {  # Plot titles
      if ('descr' %in% df$scenario) {
        title <- "Average Chars & Subgroup Shares by Participation in Read-At-Home"
      } else if ('cace' %in% names(df)) {
        title <- "Avg Complier Chars & Subgroup Shares by Assignment to Read-At-Home"
      } else {
        title <- "Average Chars & Subgroup Shares by Assignment to Read-At-Home"
      }
    } else if ('descr' %in% df$scenario) {
      title <- "Average Characteristics by Participation in Read-At-Home"
    } else if ('cace' %in% names(df)) {
      title <- "Avg Complier Characteristics by Assignment to Read-At-Home"
    } else {
      title <- "Average Characteristics by Assignment to Read-At-Home"
    }
    
    ggplot(df, aes(var, value, fill = treat_var)) +  # Plot
      geom_col(position = "dodge") +
      geom_text(
        data = filter(df, var %in% c('a_stress', 'b_value_ed', 'c_enjoy_reading')),
        aes(label = value), position = position_dodge(width = .9), vjust = 2,
        family = 'Roboto', color = "white", size = 6
      ) +
      geom_text(
        data = filter(df, !(var %in% c('a_stress', 'b_value_ed', 'c_enjoy_reading'))),
        aes(label = scales::label_percent(accuracy = 1)(prob)), 
        position = position_dodge(width = .9), vjust = 2,
        family = 'Roboto', color = "white", size = 6
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 18, hjust = .5),
        text = element_text(size = 18, family = "Roboto"),
        plot.margin = unit(c(5.5,5.5,0,5.5), "pt")
      ) +
      scale_x_discrete(labels = x_lab) +
      scale_fill_manual(
        name = leg_lab,
        values = c('Yes' = 'deepskyblue2', 'No' = 'deeppink2')
      ) +
      xlab("") + ylab("") +
      ggtitle(title)
    
  } else {  # Blank plot for observed data
    if ('descr' %in% df$scenario) {  # Legend & plot titles
      title <- "Average Characteristics by Participation in Read-At-Home: UNKNOWN"
    } else {
      title <- "Average Characteristics by Assignment to Read-At-Home: UNKNOWN"
    }
    ggplot(df) +
      theme(
        plot.title = element_text(size = 18, hjust = .5),
        text = element_text(size = 18, family = "Roboto")
      ) +
      ggtitle(title)
  }
}

# DAGs =========================================================================

df_node <- bind_rows(
  data.frame(
    id = c(1:3, 1:3, 1:4),
    label = c(
      "Participate in\nRead-At-Home", "Unobserved\nConfounders", "Reading Score",
      "Assigned to\nRead-At-Home", "Unobserved\nVariables", "Reading Score",
      "Participate in\nRead-At-Home", "Unobserved\nConfounders", "Reading Score",
      "Assigned to\nRead-At-Home"
    ),
    scenario = c(rep("descr", 3), rep("rand", 3), rep("cace", 4)),
    know = 'obs'
  ),
  data.frame(
    id = c(1:3, 1:3, 1:4),
    label = c(
      "Participate in\nRead-At-Home", "Stress, Value Edu.\n& Enjoy Read", "Reading Score",
      "Assigned to\nRead-At-Home", "Stress, Value Edu.\n& Enjoy Read", "Reading Score",
      "Participate in\nRead-At-Home", "Stress, Value Edu.\n& Enjoy Read", "Reading Score",
      "Assigned to\nRead-At-Home"
    ),
    scenario = c(rep("descr", 3), rep("rand", 3), rep("cace", 4)),
    know = 'omni'
  )
)

df_edge <- bind_rows(
  data.frame(
    from = c(1,2), to = c(3,3),
    line = c("", "dashed"),
    scenario = "all"
  ),
  data.frame(from = 2, to = 1, line = "dashed", scenario = "descr"),
  data.frame(
    from = c(2,4), to = c(1,1), line = c("dashed", ""), scenario = "cace"
  )
)

make_dag <- function(df_node, df_edge) {
  dag <- create_graph() |> 
    add_nodes_from_table(table = df_node, label_col = label) |> 
    add_edges_from_table(
      table = df_edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external
    )
  if (4 %in% df_node$id) {
    dag <- dag |> 
      set_node_position(node = 1, x = 3, y = 2) |> 
      set_node_position(node = 2, x = 4, y = 1) |> 
      set_node_position(node = 3, x = 5, y = 2) |> 
      set_node_position(node = 4, x = 1, y = 2)
  } else {
    dag <- dag |> 
      set_node_position(node = 1, x = 1, y = 2) |> 
      set_node_position(node = 2, x = 2, y = 1) |> 
      set_node_position(node = 3, x = 3, y = 2)
  }
  dag |> 
    set_node_attrs(node_attr = shape, values = "rectangle") |> 
    set_node_attrs(node_attr = fixedsize, values = F) |> 
    set_node_attrs(color, "black") |> 
    set_node_attrs(fillcolor, "white") |> 
    set_edge_attrs(color, "black") |> 
    set_node_attrs(fontname, "Roboto") |> 
    set_node_attrs(fontsize, 13) |> 
    select_nodes(id == 2) |> 
    set_node_attrs_ws(style, "dashed") |> 
    clear_selection() |> 
    select_edges(conditions = line == "dashed") |> 
    set_edge_attrs_ws(style, "dashed") |> 
    clear_selection() |> 
    render_graph()
}

# Tables =======================================================================

make_table <- function(df) {
  if ("cace" %in% names(df)) {
    if ('omni' %in% names(df)) {
      tbl <- select(df, var, No, Yes) |> 
        gt() |> 
        fmt_number(columns = 2:3, decimals = 1) |> 
        fmt_markdown(columns = 1)
    } else {
      tbl <- select(df, treat_var, reading_score, read_at_home) |> 
        tidyr::pivot_wider(
          names_from = treat_var, 
          values_from = c(reading_score, read_at_home),
          names_sep = "."
        ) |> 
        tidyr::pivot_longer(
          1:4,
          names_to = c(".values", "name"),
          names_sep = "\\."
        ) |> 
        tidyr::pivot_wider(names_from = name, values_from = value) |> 
        mutate(
          var = ifelse(
            `.values` == "reading_score", 
            "Reading Score", "Participate in<br>Read-At-Home"
          )
        ) |> 
        relocate(var) |> 
        select(-`.values`) |> 
        gt() |> 
        fmt_number(rows = 1, decimals = 1) |> 
        fmt_percent(rows = 2, decimals = 1) |> 
        fmt_markdown(columns = 1)
    }
  } else {
    tbl <- select(df, treat_var, reading_score) |> 
      tidyr::pivot_wider(names_from = treat_var, values_from = reading_score) |> 
      mutate(var = "Reading Score") |> relocate(var) |> 
      gt()
  }
  if ("rand" %in% df$scenario) {
    tbl <- tbl |> 
      tab_header(md("Avg by Assignment<br>to Read-At-Home"))
  } else {
    tbl <- tbl |> 
      tab_header(md("Avg by Participation<br>in Read-At-Home"))
  }
  tbl |> 
    cols_label(var = "", No = 'Ctrl', Yes = 'RAH') |> 
    opt_table_font("Roboto") |> 
    tab_options(heading.title.font.size = pct(100))
}

# Calculations =================================================================

df_calc <- data.frame(
  scenario = c('desc', 'rand', 'cace', 'cace'),
  know = c('both', 'both', 'obs', 'omni'),
  eqn = c(
    'Avg Treatment Effect:<br>44.2 - 29.3 = 14.9',
    'Avg Treatment Effect:<br>34.6 - 31.1 = 3.5',
    str_c(
      'Compliance Rate:<br>75.3% - 15.9% = 59.4%<br><br>',
      'Complier Avg Causal Effect:<br>(34.6 - 31.1) / 0.594 = 5.9'
    ),
    'Complier Avg Causal Effect:<br>38.4 - 31.9 = 6.5'
  )
)
