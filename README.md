# Connecticut Real-Estate Explorer (2001 – 2022)

Visit the app: **[Real Estate Sales Analysis](https://cpriyanjana.shinyapps.io/RealEstateSalesAnalysis/)**

> **An interactive Shiny dashboard analyzing 1.2M+ residential transactions from the State of Connecticut (2001–2022).**  
>  
> This unique public dataset spans multiple economic cycles—dot-com bust, 2008 crash, COVID-19 boom, and post-pandemic rate hikes—under one legal and tax regime.  
>  
> Housing is the largest household expense and a key tax revenue source. Understanding price changes over time helps planners, investors, and residents tackle affordability and forecast trends.

---

## Research Requirements

- Clean and harmonize **1.2M+** residential transactions (2001–2022).
- Summarize market activity at **yearly**, **monthly**, and **geographic** levels.
- Visualize key metrics using **colorblind-friendly** charts.
- Document **data sources**, **transformations**, and **limitations**.
- Use **tabbed views** in Shiny for focused exploration.

---

## Project Scope

- Focused on **arms-length residential transactions** only.
- **Excludes** commercial, land, and non-arms-length (e.g., foreclosures) sales.
- Visuals are limited to **county-level** to ensure performance on free Shiny hosting.
- Full methodology is commented in the code for transparency.

---

## Considerations

- **Assessed values** may lag behind actual sale prices → affects sales-ratio accuracy.
- Town names were **standardized** using fuzzy matching → some mismatches may persist.
- Limited **buyer/seller demographic data** restricts deeper analysis.

---

## Code Snippets

```r
getSales <- reactive({
  cached <- sales_cache()
  if (!is.null(cached)) return(cached)

  tf <- tempfile(fileext = ".rds")
  download.file(master_url, tf, mode = "wb")
  sales <- readRDS(tf); unlink(tf)

  sales_cache(sales)
  sales
})
##--- Market Overview (server) -------------------------------------
output$market_plot <- renderPlot({
  annual <- getAnnualStats()
  g1 <- ggplot(annual, aes(year, total_volume/1e9)) +
        geom_col() + scale_y_continuous(labels = fmt_billion)
  g2 <- ggplot(annual, aes(year, median_price/1e6)) +
        geom_line(size = 1.1) + geom_point() +
        scale_y_continuous(labels = fmt_dollar)
  g1 / g2               # patchwork vertical stack
})

##--- Town Growth dumbbell (server) --------------------------------
output$towns_plot <- renderPlot({
  ggplot(town_growth(), aes(x = price_2010, xend = price_2022,
                            y = reorder(town, price_2022))) +
    geom_dumbbell(size = 3, colour_x = "#999999", colour_xend = "#1f78b4") +
    scale_x_continuous(labels = fmt_dollar) +
    labs(x = "Median Sale Price", y = NULL)
})

 ## Season data
 
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
  
  ## Season absolute plot
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
  
  ## Mix data
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
  
  ## Mix plot
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
  
  ## Gap data
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
  
  ## Gap plot
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
