# Willamette Athletics Spending Dashboard
# Run with the "Run App" button in RStudio, or shiny::runApp()
#
# Expected folder layout (adjust DATA_DIR below if yours differs):
#   app.R
#   clean_data/
#     d3_instLevel_ds.csv, d3_sports_ds.csv, nwc_sports_ds.csv

library(shiny)
library(shinydashboard)
library(tidyverse)

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
  "Men's Basketball", "Women's Basketball"
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
wu_maroon <- "#7A1F2B"   # primary burgundy
wu_gold   <- "#E6A91C"   # accent (matches the Willamette color in nwc_colors)
wu_maroon_dark <- "#5C1720"

# Logo: the Bearcat mark lives at www/Bearcat.png (Shiny serves the www folder).
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
years_nwc   <- sort(unique(nwc_sports$year[!is.na(nwc_sports$win_pct)]), decreasing = TRUE)
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
      menuItem("Spending vs. Athletes",    tabName = "p1", icon = icon("chart-line")),
      menuItem("Spending Over Time",       tabName = "p2", icon = icon("clock")),
      menuItem("School Comparison",        tabName = "p3", icon = icon("school")),
      menuItem("Head Coach Salaries",      tabName = "p4", icon = icon("user-tie")),
      menuItem("Budget Mix",               tabName = "p5", icon = icon("chart-pie")),
      menuItem("Spending vs. Win %",       tabName = "p6", icon = icon("trophy"))
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
                  selectInput("p1_sport", "Sport", sports_all, selected = default_sport),
                  selectInput("p1_year", "Year", years_all, selected = default_year)),
              box(width = 9, plotOutput("p1_plot", height = "520px"))
      ),
      # Plot 2 -----------------------------------------------------------
      tabItem("p2",
              box(width = 3, title = "Filters", status = "danger", solidHeader = TRUE,
                  selectInput("p2_sport", "Sport", sports_all, selected = default_sport)),
              box(width = 9, plotOutput("p2_plot", height = "520px"))
      ),
      # Plot 3 -----------------------------------------------------------
      tabItem("p3",
              box(width = 3, title = "Filters", status = "danger", solidHeader = TRUE,
                  checkboxGroupInput("p3_years", "Years", choices = years_recent,
                                     selected = years_recent)),
              box(width = 9, plotOutput("p3_plot", height = "620px"))
      ),
      # Plot 4 -----------------------------------------------------------
      tabItem("p4",
              box(width = 12, plotOutput("p4_plot", height = "520px"))
      ),
      # Plot 5 -----------------------------------------------------------
      tabItem("p5",
              box(width = 3, title = "Filters", status = "danger", solidHeader = TRUE,
                  selectInput("p5_year", "Year", years_bd, selected = years_bd[1])),
              box(width = 9, plotOutput("p5_plot", height = "480px"))
      ),
      # Win pct ----------------------------------------------------------
      tabItem("p6",
              box(width = 3, title = "Filters", status = "danger", solidHeader = TRUE,
                  selectInput("p6_year", "Year", years_nwc, selected = years_nwc[1]),
                  radioButtons("p6_metric", "Spending measure",
                               choices = c("Total expenses" = "real_expense",
                                           "Operating expense per athlete" = "real_opexp_athlete")),
                  selectizeInput("p6_sports", "Sports", choices = sports_nwc,
                                 selected = sports_nwc, multiple = TRUE)),
              box(width = 9, plotOutput("p6_plot", height = "auto"))
      )
    )
  )
)

# ---- Server -------------------------------------------------------------
server <- function(input, output, session) {
  
  # Plot 1: operating expenses vs number of athletes
  output$p1_plot <- renderPlot({
    ds <- d3_sports %>%
      filter(sport == input$p1_sport, year == as.numeric(input$p1_year)) %>%
      mutate(color = case_when(
        institution_name %in% nwc ~ "Northwest Conference",
        institution_name == wu    ~ "Willamette University",
        .default = "Division III"
      ))
    validate(need(nrow(ds) > 0, "No data for this sport and year."))
    
    colorpal <- c("Willamette University" = wu_maroon,
                  "Northwest Conference"  = "blue",
                  "Division III"          = "black")
    solid  <- ds %>% filter(institution_name %in% nwc | institution_name == wu)
    others <- ds %>% filter(!(institution_name %in% nwc), institution_name != wu)
    
    ggplot(others, aes(x = n_athletes, y = op_exp, color = color)) +
      geom_point(alpha = .1) +
      geom_point(data = solid, size = 2) +
      geom_text(data = solid, aes(label = institution_name), hjust = -.1, size = 3) +
      geom_abline(intercept = 0,
                  slope = median(ds$op_exp, na.rm = TRUE) / median(ds$n_athletes, na.rm = TRUE),
                  color = "black", linetype = "dashed") +
      scale_color_manual(values = colorpal) +
      scale_x_continuous(limits = c(0, max(ds$n_athletes, na.rm = TRUE) * 1.05), expand = c(0, 0)) +
      scale_y_continuous(labels = function(x) paste0(x / 1000, "k"),
                         limits = c(0, max(ds$op_exp, na.rm = TRUE) * 1.05), expand = c(0, 0)) +
      theme_bw(base_size = 14) +
      theme(plot.caption.position = "plot") +
      labs(title = paste0("Willamette ", input$p1_sport, " vs. Other Schools in ", input$p1_year),
           subtitle = "Dashed line represents median operating expense per athlete",
           caption = caption_txt,
           x = "Number of Athletes", y = "Operating Expenses ($)", color = "Institution")
  })
  
  # Plot 2: operating expense per athlete by year
  output$p2_plot <- renderPlot({
    pts <- d3_sports %>% filter(sport == input$p2_sport, institution_name %in% nwc)
    wu_pts <- d3_sports %>% filter(sport == input$p2_sport, institution_name == wu)
    validate(need(nrow(pts) > 0, "No data for this sport."))
    
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
      labs(title = paste0("Operating Expense per Athlete: ", input$p2_sport),
           subtitle = "Willamette compared with other Northwest Conference schools",
           x = "Year", y = "Operating Expense per Athlete ($)",
           color = "Institution", caption = caption_txt)
  })
  
  # Plot 3: per-athlete spending by school (boxplots, all sports)
  output$p3_plot <- renderPlot({
    req(input$p3_years)
    d <- d3_sports %>%
      filter(institution_name %in% nwc | institution_name == wu,
             year %in% as.numeric(input$p3_years))
    validate(need(nrow(d) > 0, "No data for the selected years."))
    
    ggplot(d, aes(x = fct_reorder(institution_name, op_exp_per_athlete, .fun = median, na.rm = TRUE),
                  y = op_exp_per_athlete)) +
      geom_point(aes(color = institution_name == wu, group = institution_name),
                 alpha = .2, position = "jitter") +
      geom_boxplot(aes(color = institution_name == wu, group = institution_name), alpha = 0) +
      facet_wrap(~year) +
      theme_light(base_size = 14) +
      theme(plot.caption.position = "plot",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(labels = school_labels) +
      scale_color_manual(values = c("TRUE" = wu_maroon, "FALSE" = "gray70"), guide = "none") +
      labs(x = "School", y = "Operating Expense per Athlete ($)",
           title = "Operating Expense per Athlete by NWC School",
           subtitle = "Each dot is a sport; boxes summarize all sports for the school",
           caption = caption_txt)
  })
  
  # Plot 4: head coach salary vs year
  output$p4_plot <- renderPlot({
    d3_instLevel %>%
      mutate(HC_sal = HDCOACH_SAL_FTE_MEN + HDCOACH_SAL_FTE_WOMN) %>%
      filter(institution_name %in% nwc | institution_name == wu) %>%
      ggplot(aes(x = year, y = HC_sal)) +
      geom_line(linewidth = 1, alpha = .6,
                aes(color = institution_name == wu, group = institution_name)) +
      scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024)) +
      scale_color_manual(values = c("TRUE" = wu_maroon, "FALSE" = "gray70"),
                         labels = c("TRUE" = "Willamette", "FALSE" = "Other NWC")) +
      theme_bw(base_size = 14) +
      theme(plot.caption.position = "plot") +
      labs(title = "Head Coach Salaries Across the NWC",
           subtitle = "Total NWC head coach salaries by year",
           y = "Average Head Coach Salaries ($)", x = "Year",
           color = "School", caption = caption_txt)
  })
  
  # Plot 5: percent difference from NWC median by expense category
  output$p5_plot <- renderPlot({
    yr <- as.numeric(input$p5_year)
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
             category = recode(category,
                               operating = "Operating expenses", recruiting = "Recruiting",
                               hc_salaries = "Head coach salaries",
                               ac_salaries = "Assistant coach salaries",
                               other = "Other expenses"))
    validate(need(nrow(d) > 0, "No Willamette data for this year."))
    
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
  
  # Win pct vs spending (NWC only)
  output$p6_plot <- renderPlot({
    req(input$p6_sports)
    d <- nwc_sports %>%
      filter(!is.na(win_pct), year == as.numeric(input$p6_year),
             sport %in% input$p6_sports)
    validate(need(nrow(d) > 0, "No data for this selection."))
    
    ggplot(d, aes(x = win_pct, y = .data[[input$p6_metric]], color = institution_name)) +
      geom_point(size = 4, alpha = .8) +
      facet_wrap(~sport, scales = "free_y") +
      scale_y_continuous(labels = scales::dollar) +
      scale_color_manual(values = nwc_colors) +
      theme_light(base_size = 14) +
      labs(x = "Win percentage",
           y = if (input$p6_metric == "real_expense") "Total expenses (real $)"
           else "Operating expense per athlete (real $)",
           color = "Institution",
           title = paste0("Spending vs. Winning, ", input$p6_year))
  }, height = function() {
    n <- max(1, length(input$p6_sports))
    max(420, ceiling(n / 3) * 300)
  })
}

shinyApp(ui, server)