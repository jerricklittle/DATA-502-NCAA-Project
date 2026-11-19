# Willamette Athletics Spending Dashboard
# Run with the "Run App" button in RStudio, or shiny::runApp()
#
# Expected folder layout (adjust DATA_DIR below if yours differs):
#   app.R
#   www/Bearcat.png
#   clean_data/
#     d3_instLevel_ds.csv, d3_sports_ds.csv, nwc_sports_ds.csv

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

# ---- Data ---------------------------------------------------------------
DATA_DIR <- if (dir.exists("clean_data")) "clean_data" else "../clean_data"

d3_instLevel <- read_csv(file.path(DATA_DIR, "d3_instLevel_ds.csv"), show_col_types = FALSE)
d3_sports    <- read_csv(file.path(DATA_DIR, "d3_sports_ds.csv"),    show_col_types = FALSE)
nwc_sports   <- read_csv(file.path(DATA_DIR, "nwc_sports_ds.csv"),   show_col_types = FALSE)

# Sports shown in the dashboard (names must match the `sport` column)
keep_sports <- c(
  "Men's Football",
  "Men's Soccer", "Women's Soccer",
  "Men's Baseball", "Women's Softball",
  "Women's Lacrosse",
  "Men's Basketball", "Women's Basketball",
  "Women's Volleyball"
)
d3_sports  <- d3_sports  %>% filter(sport %in% keep_sports)
nwc_sports <- nwc_sports %>% filter(sport %in% keep_sports)

wu <- "Willamette University"
nwc <- c( # NWC teams with the exception of Willamette
  "Linfield University", "George Fox University", "Whitworth University",
  "Whitman College", "Pacific University", "University of Puget Sound",
  "Pacific Lutheran University", "Lewis & Clark College"
)

nwc_colors <- c(
  "George Fox University"       = "#002D62",
  "Lewis & Clark College"       = "#F26D21",
  "Linfield University"         = "#5B2C83",
  "Pacific Lutheran University" = "#000000",
  "Pacific University"          = "#E4002B",
  "University of Puget Sound"   = "#7F1734",
  "Whitman College"             = "#1D5FA8",
  "Whitworth University"        = "#CE1126",
  "Willamette University"       = "#E6A91C"
)

school_labels <- c(
  "Willamette University" = "WU", "University of Puget Sound" = "UPS",
  "Lewis & Clark College" = "L&C", "Pacific Lutheran University" = "PLU",
  "Pacific University" = "Pacific", "Whitworth University" = "Whitworth",
  "George Fox University" = "GFU", "Linfield University" = "Linfield",
  "Whitman College" = "Whitman"
)

# ---- Willamette theme ---------------------------------------------------
# Change these two values to re-tint the whole dashboard.
wu_maroon <- "#7A1F2B"
wu_gold   <- "#E6A91C"  
wu_maroon_dark <- "#5C1720"

# Logo
logo_src <- "Bearcat.png"

wu_css <- sprintf("
  .skin-red .main-header .logo,
  .skin-red .main-header .navbar { background-color: %1$s; }
  .skin-red .main-header .logo:hover { background-color: %2$s; }
  .skin-red .main-header .navbar .sidebar-toggle:hover { background-color: %2$s; }
  .skin-red .main-header { border-bottom: 3px solid %3$s; }
  .skin-red .main-sidebar { background-color: %2$s; }
  .skin-red .sidebar-menu > li.active > a,
  .skin-red .sidebar-menu > li:hover > a {
    background: %1$s; color: #fff; border-left-color: %3$s; }
  .skin-red .sidebar-menu > li > a { color: #f1dfe2; }
  .content-wrapper, .right-side { background-color: #faf6f6; }
  .box.box-danger { border-top-color: %3$s; }
  .box.box-solid.box-danger > .box-header { background: %1$s; background-color: %1$s; }
  .box.box-solid.box-danger { border: 1px solid %1$s; }
  .box-title, .logo { font-family: Georgia, 'Times New Roman', serif; }
  .wu-brand { display: flex; align-items: center; justify-content: center; gap: 8px; }
  .wu-brand img { height: 34px; width: auto; }
  .selectize-input.focus { border-color: %1$s; box-shadow: 0 0 4px %1$s55; }
  a { color: %1$s; }
", wu_maroon, wu_maroon_dark, wu_gold)

caption_txt <- paste0(
  "Data retrieved from the Equity in Athletics Data Analysis Cutting Tool\n",
  "by the U.S. Department of Education (https://ope.ed.gov/athletics/)."
)

# Budget breakdown used on the "Budget Mix" tab (computed once)
expense_breakdown <- d3_instLevel %>%
  filter(institution_name %in% nwc | institution_name == wu) %>%
  mutate(
    hc_salaries =
      coalesce(HDCOACH_SALARY_MEN * NUM_HDCOACH_MEN, 0) +
      coalesce(HDCOACH_SALARY_WOMEN * NUM_HDCOACH_WOMEN, 0) +
      coalesce(HDCOACH_SALARY_COED * NUM_HDCOACH_COED, 0),
    ac_salaries =
      coalesce(ASCOACH_SALARY_MEN * NUM_ASCOACH_MEN, 0) +
      coalesce(ASCOACH_SALARY_WOMEN * NUM_ASCOACH_WOMEN, 0) +
      coalesce(ASCOACH_SALARY_COED * NUM_ASCOACH_COED, 0),
    recruiting =
      coalesce(RECRUITEXP_MEN, 0) +
      coalesce(RECRUITEXP_WOMEN, 0) +
      coalesce(RECRUITEXP_COED, 0),
    operating = IL_TOTAL_OPEXP_INCLCOED,
    other = GRND_TOTAL_EXPENSE - operating - recruiting - hc_salaries - ac_salaries
  )

# Input choices
sports_all  <- sort(unique(d3_sports$sport))
years_all   <- sort(unique(d3_sports$year), decreasing = TRUE)
years_recent <- sort(unique(d3_sports$year[d3_sports$year >= 2020]))
sports_nwc  <- sort(unique(nwc_sports$sport[!is.na(nwc_sports$win_pct)]))
# Years shown on the animated win % plot
YEAR_MIN <- 2015
YEAR_MAX <- 2025
years_bd    <- sort(unique(expense_breakdown$year), decreasing = TRUE)

default_sport <- if ("Men's Football" %in% sports_all) "Men's Football" else sports_all[1]
default_year  <- if (2025 %in% years_all) 2025 else years_all[1]

# Small helper for a written takeaway box. Not currently used in the UI; to show
# one, add e.g. takeaway("Your text here") after a tab's plot box.
takeaway <- function(txt) {
  box(width = 12, status = "danger", title = "Takeaway", solidHeader = FALSE,
      collapsible = TRUE, p(txt))
}

# ---- UI -----------------------------------------------------------------
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = div(class = "wu-brand",
                img(src = logo_src, alt = "Willamette University"),
                span("WU Athletics")),
    titleWidth = 260),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Spending Over Time",       tabName = "p1", icon = icon("clock")),
      menuItem("Budget Mix",               tabName = "p2", icon = icon("chart-pie")),
      menuItem("Spending vs. Win %",       tabName = "p3", icon = icon("trophy"))
    ),
    div(style = "padding: 15px; font-size: 11px; color: #b8c7ce;",
        "Maroon = Willamette. Grey / blue = other Northwest Conference (NWC) schools.")
  ),
  dashboardBody(
    tags$head(tags$style(HTML(wu_css))),
    tabItems(
      # Plot 1 -----------------------------------------------------------
      tabItem("p1",
              box(width = 3, title = "Filters", status = "danger", solidHeader = TRUE,
                  selectInput("p1_sport", "Sport", sports_all, selected = default_sport)),
              box(width = 9, plotOutput("p1_plot", height = "520px"))
      ),
      # Plot 2 -----------------------------------------------------------
      tabItem("p2",
              box(width = 3, title = "Filters", status = "danger", solidHeader = TRUE,
                  selectInput("p2_year", "Year", years_bd, selected = years_bd[1])),
              box(width = 9, plotOutput("p2_plot", height = "480px"))
      ),
      # Plot 3 ----------------------------------------------------------
      tabItem("p3",
              box(width = 3, title = "Filters", status = "danger", solidHeader = TRUE,
                  radioButtons("p3_metric", "Spending measure",
                               choices = c("Total expenses" = "real_expense",
                                           "Operating expense per athlete" = "real_opexp_athlete")),
                  selectInput("p3_sport", "Sport", sports_nwc,
                              selected = if (default_sport %in% sports_nwc) default_sport else sports_nwc[1]),
                  helpText("Press Play or drag the slider under the plot to move through the years.")),
              box(width = 9, plotlyOutput("p3_plot", height = "560px"))
      )
    )
  )
)

# ---- Server -------------------------------------------------------------
server <- function(input, output, session) {
  
  # Plot 1: operating expense per athlete by year
  output$p1_plot <- renderPlot({
    pts <- d3_sports %>% filter(sport == input$p1_sport, institution_name %in% nwc)
    wu_pts <- d3_sports %>% filter(sport == input$p1_sport, institution_name == wu)
    shiny::validate(need(nrow(pts) > 0, "No data for this sport."))
    
    ggplot(pts, aes(x = year, y = op_exp_per_athlete)) +
      geom_point(alpha = .4, aes(color = "Other NWC Schools")) +
      geom_smooth(se = FALSE, alpha = .5, aes(color = "Other NWC Schools"),
                  method = "loess", formula = y ~ x) +
      geom_point(data = wu_pts, aes(color = "Willamette University")) +
      geom_smooth(data = wu_pts, se = FALSE, aes(color = "Willamette University"),
                  method = "loess", formula = y ~ x) +
      theme_light(base_size = 14) +
      theme(plot.caption.position = "plot") +
      scale_color_manual(values = c("Willamette University" = wu_maroon,
                                    "Other NWC Schools" = "grey70")) +
      labs(title = paste0("Operating Expense per Athlete: ", input$p1_sport),
           subtitle = "Willamette compared with other Northwest Conference schools",
           x = "Year", y = "Operating Expense per Athlete ($)",
           color = "Institution", caption = caption_txt)
  })
  
  # Plot 2: percent difference from NWC median by expense category
  output$p2_plot <- renderPlot({
    yr <- as.numeric(input$p2_year)
    d <- expense_breakdown %>%
      filter(year == yr) %>%
      mutate(across(c(operating, recruiting, hc_salaries, ac_salaries, other),
                    ~ .x / GRND_TOTAL_EXPENSE * 100)) %>%
      select(institution_name, operating, recruiting, hc_salaries, ac_salaries, other) %>%
      pivot_longer(-institution_name, names_to = "category", values_to = "percentage") %>%
      group_by(category) %>%
      mutate(nwc_median = median(percentage[institution_name %in% nwc], na.rm = TRUE)) %>%
      ungroup() %>%
      filter(institution_name == wu) %>%
      mutate(difference = percentage - nwc_median,
             category = dplyr::recode(category,
                                      operating = "Operating expenses", recruiting = "Recruiting",
                                      hc_salaries = "Head coach salaries",
                                      ac_salaries = "Assistant coach salaries",
                                      other = "Other expenses"))
    shiny::validate(need(nrow(d) > 0, "No Willamette data for this year."))
    
    ggplot(d, aes(x = difference, y = reorder(category, difference), fill = difference > 0)) +
      geom_col() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_x_continuous(labels = \(x) paste0(x, " pp")) +
      scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = wu_maroon)) +
      theme_light(base_size = 14) +
      theme(legend.position = "none", plot.caption.position = "plot") +
      labs(title = paste0("Willamette's Budget Mix vs. NWC Median, ", yr),
           subtitle = "Share of total athletics expenses, difference in percentage points",
           x = "Difference in percentage points", y = "Expense Category",
           caption = caption_txt)
  })
  
  # Plot 3: Win pct vs spending, animated by year (NWC schools + Willamette)
  output$p3_plot <- renderPlotly({
    req(input$p3_sport)
    metric_label <- if (input$p3_metric == "real_expense") "Total expenses (real $)"
    else "Operating expense per athlete (real $)"
    
    d <- nwc_sports %>%
      filter(!is.na(win_pct),
             year >= YEAR_MIN, year <= YEAR_MAX,
             sport == input$p3_sport,
             institution_name %in% c(nwc, wu)) %>%
      mutate(spend = .data[[input$p3_metric]],
             group = factor(if_else(institution_name == wu,
                                    "Willamette University", "Other NWC Schools"),
                            levels = c("Other NWC Schools", "Willamette University")),
             hover = paste0("<b>", institution_name, "</b><br>",
                            "Year: ", year, "<br>",
                            "Win %: ", round(win_pct, 3), "<br>",
                            metric_label, ": ", scales::dollar(spend))) %>%
      filter(!is.na(spend)) %>%
      arrange(year, group)
    shiny::validate(need(nrow(d) > 0, "No data for this selection."))
    
    plot_ly(
      d,
      x = ~win_pct, y = ~spend,
      color = ~group,
      colors = c("Other NWC Schools" = "grey70", "Willamette University" = wu_maroon),
      frame = ~year,
      ids = ~institution_name,      # keeps each school's point identity so it glides between years
      text = ~hover, hoverinfo = "text",
      type = "scatter", mode = "markers",
      marker = list(size = 14, opacity = 0.8, line = list(color = "white", width = 1))
    ) %>%
      layout(
        title = list(text = paste0("Spending vs. Winning, ", input$p3_sport)),
        xaxis = list(title = "Win percentage",
                     range = range(d$win_pct) + c(-0.05, 0.05) * diff(range(d$win_pct))),
        yaxis = list(title = metric_label, tickprefix = "$", tickformat = ",.0f",
                     range = c(0, max(d$spend) * 1.1)),
        legend = list(title = list(text = "Institution"))
      ) %>%
      animation_opts(frame = 900, transition = 700, easing = "cubic-in-out", redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Year: ", font = list(color = wu_maroon))) %>%
      animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)