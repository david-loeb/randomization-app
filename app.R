#==============================================================================#
# Causal Inference With Randomization App                                      #
#==============================================================================#

library(shiny); library(bslib); library(shinyWidgets); library(DT)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 130)  # necessary for font size to stay consistent
sysfonts::font_add('Roboto', 'www/Roboto-Medium.ttf')
source('setup.R')

# User Interface ===============================================================

ui <- page_navbar(
  navbar_options = list(bg = "#5539CC", theme = 'dark'),
  header = tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  theme = bs_theme(
    base_font = font_face(
      family = "Roboto",
      src = "url('../Roboto-Medium.ttf') format('truetype')"
    ),
    heading_font = font_face(
      family = "Roboto",
      src = "url('../Roboto-Medium.ttf') format('truetype')"
    )
  ),
  title = 'Randomizing for Causal Inference: Read-At-Home Example',
  nav_spacer(),
  
  ## Sidebar -------------------------------------------------------------------
  
  sidebar = sidebar(
    prettyRadioButtons(
      inputId = "scenario",
      label = "Scenario",
      choices = c("Descriptive", "Randomized", "Compliance"),
      selected = "Descriptive",
      status = 'success'
    ),
    prettyRadioButtons(
      inputId = "know",
      label = "Data",
      choices = c("Observed", "Omniscient"),
      selected = "Observed",
      status = 'success'
    ),
    prettyCheckbox(
      inputId = "strata",
      label = "Subgroups",
      status = 'success'
    ),
    width = 160,
    bg = "#2b2f36"
  ),
  
  ## Content -------------------------------------------------------------------
  
  nav_panel(
    'Effect Estimates',
    layout_columns(
      layout_columns(
        card(plotOutput('ate')),
        grVizOutput('dag'),
        col_widths = c(12, 12),
        row_heights = c(5, 2)
      ),
      card(
        'Treatment Effect Calculation',
        layout_columns(
          gt_output('tbl'),
          htmlOutput('calc'),
          col_widths = c(12, 12),
          row_heights = c(4,2),
          gap = '.5rem'
        )
      ),
      card(plotOutput('bal')),
      col_widths = c(7, 5, 12),
      row_heights = c(5, 3)
    )
  ),
  nav_panel(
    'Dataset',
    DTOutput('dataset')
  )
)

# Server =======================================================================

server <- function(input, output) {
  
  ## ATE Plot ------------------------------------------------------------------
  
  output$ate <- renderPlot({  # Filter
    df_plt_ate <- switch(
      input$scenario,
      "Descriptive" = filter(df, scenario == "descr"),
      "Randomized" = filter(df, scenario == "rand"),
      "Compliance" = filter(df, scenario == "rand")
    )
    make_plot_ate(df_plt_ate)  # Plot
  })
  
  ## DAG -----------------------------------------------------------------------
  
  output$dag <- renderGrViz({
    df_node_dag <- switch(  # Filter
      input$scenario,
      "Descriptive" = filter(df_node, scenario == 'descr'),
      "Randomized" = filter(df_node, scenario == 'rand'),
      "Compliance" = filter(df_node, scenario == 'cace')
    )
    df_node_dag <- switch(  # Filter
      input$know,
      "Observed" = filter(df_node_dag, know == 'obs'),
      "Omniscient" = filter(df_node_dag, know == 'omni')
    )
    df_edge_dag <- switch(
      input$scenario,
      "Descriptive" = filter(df_edge, scenario %in% c('descr', 'all')),
      "Randomized" = filter(df_edge, scenario == 'all'),
      "Compliance" = filter(df_edge, scenario %in% c('cace', 'all'))
    )
    make_dag(df_node_dag, df_edge_dag)  # DAG
  })
  
  ## Table ---------------------------------------------------------------------
  
  output$tbl <- render_gt({
    outputArgs = list(  # Filters
      df_tbl <- switch(
        input$scenario,
        "Descriptive" = filter(df, scenario == "descr"),
        "Randomized" = filter(df, scenario == "rand"),
        "Compliance" = filter(df, scenario == "rand") |> mutate(cace = 1)
      ),
      df_tbl <- switch(
        input$know,
        "Observed" = df_tbl,
        "Omniscient" = if ('cace' %in% names(df_tbl)) {
          mutate(df_cace, cace = 1, omni = 1, scenario = 'rand')
        } else {
          df_tbl
        }
      )
    )
    expr = make_table(df_tbl)  # Table
  })
  
  ## Cacluations ---------------------------------------------------------------
  
  output$calc <- renderUI({
    df_calc_eqn <- switch(  # Filters
      input$scenario,
      "Descriptive" = filter(df_calc, scenario == 'desc'),
      "Randomized" = filter(df_calc, scenario == 'rand'),
      "Compliance" = filter(df_calc, scenario == 'cace')
    )
    df_calc_eqn <- switch(
      input$know,
      "Observed" = if ('cace' %in% df_calc_eqn$scenario) {
        filter(df_calc_eqn, know == 'obs')
      } else {
        df_calc_eqn
      },
      "Omniscient" = if ('cace' %in% df_calc_eqn$scenario) {
        filter(df_calc_eqn, know == 'omni')
      } else {
        df_calc_eqn
      }
    )
    HTML(df_calc_eqn$eqn)  # Equation
  })
  
  ## Balance Plot --------------------------------------------------------------
  
  output$bal <- renderPlot({  # Filter
    df_plt_bal <- switch(
      input$scenario,
      "Descriptive" = filter(df_long, scenario == 'descr'),
      "Randomized" = filter(df_long, scenario == 'rand'),
      "Compliance" = filter(df_long, scenario == 'rand') |> mutate(cace = 1)
    )
    df_plt_bal <- switch(
      input$know,
      "Observed" = df_plt_bal,
      "Omniscient" = if ('cace' %in% names(df_plt_bal)) {
        mutate(df_long_cace, omni = 1, cace = 1)
      } else {
        mutate(df_plt_bal, omni = 1)
      }
    )
    df_plt_bal <- if (input$strata) mutate(df_plt_bal, strata = 1) else df_plt_bal
    make_plot_bal(df_plt_bal)  # Plot
  })
  
  ## Dataset -------------------------------------------------------------------
  
  df_full_dt <- reactive({
    if (input$scenario == "Descriptive") {
      if (input$know == "Observed") {
        select(
          df_full, id, read_at_home = read_at_home_descr,
          reading_score = reading_score_descr
        )
      } else if (input$know == "Omniscient") {
        select(
          df_full, id, read_at_home = read_at_home_descr, subgroup,
          reading_score = reading_score_descr, 
          value_ed, enjoy_reading, stress, prob_read_at_home
        )
      }
    } else if (input$scenario == "Randomized") {
      if (input$know == "Observed") {
        select(
          df_full, id, random_assignment, read_at_home = read_at_home_rand,
          reading_score = reading_score_rand
        )
      } else if (input$know == "Omniscient") {
        select(
          df_full, id, random_assignment, read_at_home = read_at_home_rand,
          subgroup, reading_score = reading_score_rand,
          value_ed, enjoy_reading, stress, prob_read_at_home
        )
      }
    } else if (input$scenario == "Compliance") {
      if (input$know == "Observed") {
        select(
          df_full, id, random_assignment, read_at_home = read_at_home_rand,
          subgroup, reading_score = reading_score_rand
        ) |>
          mutate(
            subgroup = ifelse(
              subgroup == 'always taker' & random_assignment == 0 |
                subgroup == 'never taker' & random_assignment == 1,
              subgroup, NA
            )
          )
      } else if (input$know == "Omniscient") {
        select(
          df_full, id, random_assignment, read_at_home = read_at_home_rand,
          subgroup, reading_score = reading_score_rand,
          value_ed, enjoy_reading, stress, prob_read_at_home
        )
      }
    }
  })
  
  output$dataset <- renderDT({
    req(input$scenario)
    req(input$know)
    datatable(
      data = df_full_dt(),
      options = list(
        pageLength = 200,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
  
}

shinyApp(ui = ui, server = server)