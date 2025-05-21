# app.R – Shiny application for Connecticut Real‑Estate Sales 2001‑2022
# ------------------------------------------------------------------------------
# Author : Priyanjana Chaudhary
# Purpose: Interactive exploration of Connecticut real‑estate transactions 2001–2022
# ------------------------------------------------------------------------------

# ---------------------------- 0.  PACKAGES ------------------------------------
if (!file.exists('packrat/init.R')) {
  libs <- c(
    "shiny", "tidyverse", "lubridate", "janitor", "sf", "glue", "scales",
    "ggplot2", "slider", "patchwork", "ggalt", "viridis", "kableExtra", "cowplot"
  )
  
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  for(pkg in libs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste('Installing package:', pkg))
      tryCatch({
        install.packages(pkg)
      }, error = function(e) {
        warning(paste('Failed to install package:', pkg, '- Error:', e$message))
      })
    }
  }
}

load_pkg <- function(pkg) {
  success <- tryCatch({
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    TRUE
  }, error = function(e) {
    message(paste('Error loading package:', pkg, '- Will try alternatives if available'))
    FALSE
  })
  return(success)
}

if (!load_pkg('shiny')) {
  stop("Cannot load 'shiny' package. App cannot run.")
}

if (!load_pkg('tidyverse')) {
  tidyverse_core <- c('ggplot2', 'dplyr', 'tidyr', 'readr', 'purrr', 'tibble', 'stringr', 'forcats')
  sapply(tidyverse_core, load_pkg)
}

other_pkgs <- c('lubridate', 'janitor', 'sf', 'glue', 'scales', 'slider', 
                'patchwork', 'ggalt', 'viridis', 'kableExtra', 'cowplot')
loaded_status <- sapply(other_pkgs, load_pkg)

# ---------------------------- 1.  DATA ----------------------------------------
cached_data <- reactiveVal(NULL)
cached_annual <- reactiveVal(NULL)
cached_town_growth <- reactiveVal(NULL)
cached_season <- reactiveVal(NULL)
cached_mix <- reactiveVal(NULL)
cached_gap_tbl <- reactiveVal(NULL)

# Helpers
fmt_dollar <- label_dollar(scale_cut = cut_si("M"), accuracy = 0.1)
fmt_billion <- label_number(scale_cut = cut_si("B"), accuracy = 0.1)

# ---------------------------- 2. UI DEFINITION --------------------------------
ui <- navbarPage(
  title = "Connecticut Real‑Estate Sales 2001‑2022",
  
  # ---- 4·1  Overview ----------------------------------------------------------
  tabPanel("Overview",
           h2("Research Overview"),
           p("This project analyses 22 years of public real‑estate transaction data",
             "from the State of Connecticut to explore long‑term price trends,",
             "housing‑mix shifts, and geographical disparities. We selected this domain",
             "because housing affordability is a critical economic and social issue,",
             "and Connecticut provides a well‑maintained statewide registry with enough",
             "history to observe multiple market cycles."),
           br(),
           h3("Key Questions Addressed"),
           tags$ol(
             tags$li("How have total sales volumes and median prices evolved since 2001?"),
             tags$li("Which towns have appreciated fastest post‑Great Recession?"),
             tags$li("What seasonal patterns exist in sale prices?"),
             tags$li("How has the property‑type mix changed over time?"),
             tags$li("Where are condo vs. single‑family gaps widest?")
           )
  ),
  
  # ---- 4·2  Research Requirements -------------------------------------------
  tabPanel("Requirements",
           h2("Research Requirements"),
           p("The study was guided by the following explicit requirements:"),
           tags$ul(
             tags$li("Clean and harmonise 1.2 M+ transaction records from 2001‑2022"),
             tags$li("Summarise market activity at yearly, monthly, and geographic levels"),
             tags$li("Visualise key metrics with clear, colour‑blind‑friendly charts"),
             tags$li("Document data sources, transformations, and limitations inline"),
             tags$li("Separate visual outputs across multiple Shiny tabs to aid user focus")
           )
  ),
  
  # ---- 4·3  Project Scope -----------------------------------------------------
  tabPanel("Scope & Documentation",
           h2("Scope of Project"),
           p("The application focuses exclusively on arms‑length residential transactions",
             "recorded by Connecticut municipalities. Commercial, vacant‑land, and",
             "non‑arms‑length transfers (e.g., foreclosures) were excluded from price",
             "trend analyses. Spatial mapping is limited to county‑level summaries to",
             "keep data size manageable for the free shinyapps.io tier. Full methodological",
             "details are included as comments in the source code."),
           h3("Known Limitations"),
           tags$ul(
             tags$li("Assessed values occasionally lag sale prices, affecting sales‑ratio calculations."),
             tags$li("Town names were standardised via string matching; rare mismatches may persist."),
             tags$li("Missing buyer/seller type restricts deeper demographic analysis.")
           )
  ),
  
  # ---- 4·4  Visualisations ----------------------------------------------------
  navbarMenu("Visualisations",
  tabPanel(
    "Market Overview",
    fluidRow(
      column(
        width = 12,
        h2("Connecticut Real Estate Market Trends", align = "center"),
        plotOutput("market_plot", height = "600px")
      )
    )
  ),
  
  # Panel 2: Town Growth
  tabPanel(
    "Town Growth",
    fluidRow(
      column(
        width = 12,
        h2("Fastest-Appreciating Towns", align = "center"),
        plotOutput("towns_plot", height = "600px")
      )
    )
  ),
  
  # Panel 3: Seasonal Analysis
  tabPanel(
    "Seasonality",
    fluidRow(
      column(
        width = 12,
        h2("Seasonal Price Variations", align = "center"),
        fluidRow(
          column(width = 6, plotOutput("season_abs_plot")),
          column(width = 6, plotOutput("season_dev_plot"))
        )
      )
    )
  ),
  
  # Panel 4: Housing Mix
  tabPanel(
    "Housing Mix",
    fluidRow(
      column(
        width = 12,
        h2("Housing Mix Over Time", align = "center"),
        plotOutput("mix_plot", height = "500px")
      )
    )
  ),
  
  # Panel 5: Price Gap
  tabPanel(
    "Condo vs. Single-Family",
    fluidRow(
      column(
        width = 12,
        h2("Price Gap Analysis", align = "center"),
        plotOutput("gap_plot", height = "600px")
      )
    )
  )),
  
  # About Panel
  tabPanel(
    "About",
    fluidRow(
      column(
        width = 12,
        h2("About This Application", align = "center"),
        p("This Shiny app provides interactive exploration of Connecticut real-estate transactions from 2001 to 2022."),
        p("Data source: Connecticut state records, processed and cleaned for analysis."),
        p("Created by Priyanjana Chaudhary")
      )
    )
  )
)

# ---------------------------- 3. SERVER DEFINITION ----------------------------
server <- function(input, output, session) {
  
  getData <- reactive({
    if (!is.null(cached_data())) {
      return(cached_data())
    }
    
    withProgress(message = 'Loading data...', value = 0.5, {
      master_url <- "https://raw.githubusercontent.com/cpriyanjana/RealEstateSalesAnalysis/main/sales_cleaned.rds"
      tf <- tempfile(fileext = ".rds")
      download.file(master_url, tf, mode = "wb")
      sales <- readRDS(tf)
      unlink(tf)
      cached_data(sales)
      return(sales)
    })
  })
  
  # Process annual data
  getAnnualData <- reactive({
    if (!is.null(cached_annual())) {
      return(cached_annual())
    }
    
    sales <- getData()
    annual <- sales %>%
      mutate(year = year(date_recorded)) %>%
      group_by(year) %>%
      summarise(
        total_volume = sum(sale_amount, na.rm = TRUE),
        median_price = median(sale_amount, na.rm = TRUE),
        .groups = "drop"
      )
    
    cached_annual(annual)
    return(annual)
  })
  
  # Market overview plot
  output$market_plot <- renderPlot({
    annual <- getAnnualData()
    
    # Plot A: Total dollar volume
    g_vol <- ggplot(annual, aes(year, total_volume / 1e9)) +
      geom_col(fill = viridis_pal(option = "C")(3)[2], width = 0.8) +
      scale_y_continuous(labels = fmt_billion) +
      labs(
        y = "Total Volume (billion USD)", x = NULL,
        title = "Connecticut Real‑Estate Market (2001 – 2022)",
        subtitle = "Total recorded sales volume each calendar year"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title.position = "plot")
    
    # Plot B: Median price
    g_med <- ggplot(annual, aes(year, median_price / 1e6)) +
      geom_line(linewidth = 1.2, colour = viridis_pal(option = "A")(3)[1]) +
      geom_point(colour = viridis_pal(option = "A")(3)[3], size = 2) +
      scale_y_continuous(labels = fmt_dollar) +
      labs(y = "Median Price (million USD)", x = NULL) +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(t = 0))
    
    g_vol / g_med + plot_layout(heights = c(1.1, 1))
  })
  
  # Town growth data
  getTownGrowthData <- reactive({
    if (!is.null(cached_town_growth())) {
      return(cached_town_growth())
    }
    
    sales <- getData()
    town_growth <- sales %>%
      filter(year(date_recorded) %in% c(2010, 2022)) %>%
      group_by(town, yr = year(date_recorded)) %>%
      summarise(median_price = median(sale_amount), .groups = "drop") %>%
      pivot_wider(names_from = yr, values_from = median_price, names_prefix = "yr_") %>%
      mutate(
        pct_change = (yr_2022 - yr_2010) / yr_2010,
        town       = fct_reorder(town, pct_change),
        pct_lbl    = scales::percent(pct_change, accuracy = 1),
        yr_2010_m  = yr_2010 / 1e6,
        yr_2022_m  = yr_2022 / 1e6
      ) %>%
      slice_max(pct_change, n = 10, with_ties = FALSE)
    
    cached_town_growth(town_growth)
    return(town_growth)
  })
  
  # Town growth plot
  output$towns_plot <- renderPlot({
    town_growth <- getTownGrowthData()
    
    ggplot(town_growth, aes(y = town)) +
      geom_dumbbell(aes(x = yr_2010_m, xend = yr_2022_m),
                    size = 3,
                    colour    = "grey70",
                    colour_x  = viridis(1, option = "A"),
                    colour_xend = viridis(1, option = "C")) +
      geom_text(aes(x = yr_2022_m + 0.12, label = pct_lbl),
                size = 3, hjust = 0, family = "sans") +
      scale_x_continuous(labels = scales::label_dollar(suffix = " M"),
                         expand = expansion(mult = c(.01, .15))) +
      labs(
        x       = "Median Sale Price",
        y       = NULL,
        title   = "Fastest‑Appreciating Connecticut Towns (2010 → 2022)",
        subtitle = "Top‑10 by % increase in median residential sale price"
      ) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank(),
            plot.title.position = "plot")
  })
  
  # Season data
  getSeasonData <- reactive({
    if (!is.null(cached_season())) {
      return(cached_season())
    }
    
    sales <- getData()
    season <- sales %>%
      mutate(year = year(date_recorded), month = month(date_recorded, label = TRUE, abbr = TRUE)) %>%
      group_by(year, month) %>%
      summarise(median_price = median(sale_amount), .groups = "drop") %>%
      group_by(month) %>%
      mutate(delta_vs_norm = (median_price / median(median_price)) - 1) %>%
      ungroup()
    
    season$month <- factor(season$month, levels = month.abb)
    
    cached_season(season)
    return(season)
  })
  
  # Season absolute plot
  output$season_abs_plot <- renderPlot({
    season <- getSeasonData()
    
    ggplot(season, aes(month, factor(year), fill = median_price / 1e3)) +
      geom_tile(colour = "grey95", linewidth = .3) +
      scale_fill_viridis_c(option = "C", name = "Median price\n($ 000)",
                           labels = label_dollar(suffix = " k", accuracy = 1)) +
      scale_y_discrete(expand = expansion(add = .3)) +
      labs(title = "Median Sale Price by Month",
           subtitle = "darker = higher $",
           x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1),
            panel.grid = element_blank())
  })
  
  # Season deviation plot
  output$season_dev_plot <- renderPlot({
    season <- getSeasonData()
    
    ggplot(season, aes(month, factor(year), fill = delta_vs_norm)) +
      geom_tile(colour = "grey95", linewidth = .3) +
      scale_fill_viridis_c(option = "A", limits = c(-.25, .25),
                           name = "% vs.\nmonth norm",
                           labels = percent_format(accuracy = 1)) +
      scale_y_discrete(expand = expansion(add = .3)) +
      labs(title = "Deviation from Monthly Average",
           subtitle = "% compared to month's typical price",
           x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1),
            panel.grid = element_blank())
  })
  
  # Mix data
  getMixData <- reactive({
    if (!is.null(cached_mix())) {
      return(cached_mix())
    }
    
    sales <- getData()
    top_n <- 4
    mix <- sales %>%
      mutate(property_type = fct_lump_n(property_type, n = top_n, other_level = "Other")) %>%
      count(year = year(date_recorded), property_type, name = "n") %>%
      group_by(year) %>% mutate(share = n / sum(n)) %>% ungroup() %>%
      complete(year = full_seq(year, 1), property_type, fill = list(share = 0)) %>%
      arrange(property_type, year) %>%
      group_by(property_type) %>% 
      mutate(share_smooth = slider::slide_dbl(share, mean, .before = 1, .after = 1, na.rm = TRUE)) %>% 
      ungroup()
    
    latest_order <- mix %>% 
      filter(year == max(year)) %>% 
      arrange(desc(share_smooth)) %>% 
      pull(property_type)
    
    mix$property_type <- factor(mix$property_type, levels = rev(latest_order))
    
    cached_mix(list(mix = mix, top_n = top_n))
    return(list(mix = mix, top_n = top_n))
  })
  
  # Mix plot
  output$mix_plot <- renderPlot({
    mix_data <- getMixData()
    mix <- mix_data$mix
    top_n <- mix_data$top_n
    
    ggplot(mix, aes(year, property_type, fill = share_smooth)) +
      geom_tile(colour = "white", linewidth = 0.3) +
      scale_x_continuous(breaks = pretty_breaks(n = 8), expand = c(0,0)) +
      scale_fill_viridis_c(option = "C", labels = percent_format(accuracy = 1), name = "% of\ntransactions") +
      labs(title = "Housing Mix • Heat‑map View (2001 – 2022)",
           subtitle = glue::glue("Top {top_n} property categories • 3‑year rolling share"),
           x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1), panel.grid = element_blank())
  })
  
  # Gap data
  getGapData <- reactive({
    if (!is.null(cached_gap_tbl())) {
      return(cached_gap_tbl())
    }
    
    sales <- getData()
    geo_var <- if ("county" %in% names(sales)) "county" else "town"
    
    gap_tbl <- sales %>%
      mutate(category = case_when(
        str_detect(property_type, regex("condo",  TRUE)) ~ "Condo",
        str_detect(property_type, regex("single", TRUE)) ~ "Single‑Family",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(category)) %>%
      group_by(across(all_of(geo_var)), category) %>%
      summarise(median_price = median(sale_amount), .groups = "drop") %>%
      pivot_wider(names_from = category, values_from = median_price) %>%
      mutate(gap = `Single‑Family` - Condo, abs_gap = abs(gap)) %>%
      drop_na(gap) %>% arrange(desc(abs_gap)) %>% slice_head(n = 20)
    
    cached_gap_tbl(list(gap_tbl = gap_tbl, geo_var = geo_var))
    return(list(gap_tbl = gap_tbl, geo_var = geo_var))
  })
  
  # Gap plot
  output$gap_plot <- renderPlot({
    gap_data <- getGapData()
    gap_tbl <- gap_data$gap_tbl
    geo_var <- gap_data$geo_var
    
    ggplot(gap_tbl, aes(y = reorder(.data[[geo_var]], abs_gap))) +
      geom_dumbbell(aes(x = Condo, xend = `Single‑Family`, colour = gap > 0),
                    size = 3, colour_x  = viridis(2, option = "C")[1],
                    colour_xend = viridis(2, option = "C")[2], dot_guide = TRUE, dot_guide_size = .3) +
      scale_colour_manual(values = c(`TRUE` = NA, `FALSE` = NA), guide = "none") +
      scale_x_continuous(labels = label_dollar()) +
      labs(title = "Condo vs. Single‑Family Median Prices",
           subtitle = glue::glue("Top 20 {str_to_title(geo_var)}s by absolute price gap"),
           x = "Median Sale Price (USD)", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank(), plot.title.position = "plot")
  })
}

# ---------------------------- 4. RUN APP --------------------------------------
shinyApp(ui = ui, server = server)
