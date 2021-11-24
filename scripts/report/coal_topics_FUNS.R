

main_eras <- c(1935, 1963, 2005)


party_names <- c("Country", "Greens", "Labor", "Liberal", "Other")

party_colours <- c("goldenrod3", "darkgreen", "firebrick", "blue", "black")


category_colours <- rev(c(workers = "darkred", trade = "blue", supply = "black", 
                          politics = "darkorange", environment = "darkgreen",
                          economy = "purple"))


names(party_colours) <- party_names




aus_pms <- fread.("/data/hansard/terms/aus_pm_parties_clean.csv") %>% 
  arrange.(start_year) %>% 
  mutate.(end_year = lead(start_year, default = 2022),
          simple_party = fct_relevel(ifelse.(party == "Australian Labor Party", "Labor", "Liberal"), "Labor"))


elections <- tibble(year = elec_period) %>% 
  left_join.(select(aus_pms, start_year, pm), by = c("year" = "start_year")) %>% 
  fill(pm) %>% 
  mutate.(legislature = cumsum(replace_na(pm != lag(pm), TRUE)),
          era = cut(year, c(1900, main_eras, 2021)))


short_eras_df <- tibble(min_age = c(1901, main_eras),
                  max_age = c(main_eras, 2020),
                  name = c("supply", "work rights", "diversification", "Kyoto"),
                  # color = c("#D4CB92", "#96031A", "#587792", "#AECAA1")
)

long_eras_df <- tibble(min_age = c(1901, 1980),
                  max_age = c(1980, 2020),
                  name = c("First modernity", "Second modernity"),
                  color = c("#D4CB92", "beige")
)


eras_add_on <- list(coord_geo(dat = list(short_eras_df, long_eras_df), 
          size = list(3, 3), 
          pos = list('bottom', 'bottom'),
          height = list(unit(1, "lines"),
                        unit(1, "lines"))))


plot_gov_add_on <- list(geom_rect(data = filter(aus_pms, 
                                                start_year > minimum_display_year), colour=NA, show.legend=FALSE,
                                  aes(xmin=start_year, xmax= end_year, ymin = -Inf, ymax = Inf,
                                      fill=simple_party), alpha=0.15))


basic_add_ons <- list(scale_fill_manual(values = party_colours),
                      scale_x_continuous(
                        # limits = c(1901, 2021),
                                         breaks = c(1901, seq(1925, 2000, 
                                                              by = 25), 
                                                    2020),
                                         expand = expansion(add = c(2, 2))
                                         ),
                      labs(x = ""),
                      geom_vline(xintercept = main_eras, linetype = "dashed"),
                      theme(
                        # panel.background = element_blank(),
                        #     panel.grid.major = element_line(size = 0.1, 
                        #                                     color = "grey30"),
                        text = element_text(family="Serif"))
                      )



tax_add_ons <- list(labs(x = "", y = ""),
                    scale_colour_manual(
                      values = c(
                        "Construction" = "darkgreen",
                        "Manufacturing" = "orange",
                        "Mining" = "blue",
                        "Education" = "deeppink")
                    ),
                    scale_x_continuous(labels = label_number(accuracy = 1, 
                                                             big.mark = ""), 
                                       expand = c(0.01, 0.2)),
                    theme(text = element_text(size=20, family="Serif")))



simple_series_name <- function(str) {
  
  str %>% 
    str_remove_all("[^A-Za-z- ]") %>% 
    str_squish() %>% 
    str_extract("^[a-zA-Z]+") %>% 
    str_replace_all(c("Natural" = "Natural gas", 
                      "Iron" = "Iron ore",
                      "Metal" = "Metal ores",
                      "Mining" = "All mining",
                      "Tertiary" = "Higher ed.",
                      "Education" = "Higher ed."))
}

check_series_name <- function(df, nm = "\\bcoal\\b") {
  
  df %>%
    filter.(str_detect(series, regex(nm, ignore_case = T))) %>%
    distinct.(table_title, series, series_id, unit) %>%
    View(paste0("series containing the regex", nm))
  
}

ellipsis_to_vector <- function(...) {
  # Convert to a list, but with the variables still as an expression
  args_as_expression = substitute(list(...))
  # Deparse each item in the expression, turning it into a char vector
  # Why the [-1]? Because the first element in the list expression is "list"! :-)
  args_as_char_vector = sapply(args_as_expression,deparse)[-1]
  args_as_char_vector
}


quick_plot_series <- function(df, nm, ...) {
  
  
  series_vec <- ellipsis_to_vector(...)
  
  
  df %>%
    filter.(series_id %in% series_vec) %>%
    mutate.(series = str_trunc(series, 20)) %>%
    ggplot() +
    geom_col(aes(year, value, fill = series)) +
    facet_wrap(~series_id) +
    labs(subtitle = nm, x = "", y = "")
  
}


plot_differences <- function(main_var) {
  
  
  main_score_df <- all_topic_scores_df %>% 
    summarize.(.by = {{ main_var }},
               main_score = sum(score))
  
  
  get_diffs_by <- function(group_var) {
    
    group_scores <- all_topic_scores_df %>% 
      summarize.(.by = {{group_var}}, party_total_score = sum(score))
    
    all_topic_scores_df %>% 
      summarize.(.by = c({{group_var}}, {{ main_var }}, category),
                 party_lab_score = sum(score)) %>% 
      left_join.(group_scores) %>% 
      mutate.(lab_prop_party = party_lab_score/party_total_score) %>% 
      mutate.(.by = {{ main_var }}, lab_av_prop = mean(lab_prop_party)) %>% 
      mutate.(diff = (lab_prop_party - lab_av_prop) * 100,
              diff_abs = abs(diff)) %>% 
      filter(!is.na(category)) %>% 
      rename(common_group = {{ group_var }})
    
  }
  
  
  lab_props_party <- get_diffs_by(final_alliance)
  
  labor_props_region <- get_diffs_by(labor_mining_region) %>% 
    mutate.(common_group = ifelse.(common_group == TRUE, 
                                   "Labor coal", 
                                   NA_character_)) %>% 
    filter.(!is.na(common_group))
  
  country_props_region <- get_diffs_by(country_mining_region) %>% 
    mutate.(common_group = ifelse.(common_group == TRUE, 
                                   "Country coal", 
                                   NA_character_)) %>% 
    filter.(!is.na(common_group))
  
  
  # browser()
  
  cat("Differences range:", range(lab_props_party$diff), "\n")
  
  
  
  add_formatting <- function(s) {
    case_when(
      s == "Labor coal" ~ "<span style='font-size:7pt;color:white'>Labor<br>coal</span>",
      s == "Country coal" ~ "<span style='font-size:7pt;color:white'>Country<br>coal</span>",
      TRUE ~ as.character(glue("<span style='font-size:8pt;color:white'>{s}</span>")))
  }
  
  
  party_names <- add_formatting(party_names)
  
  names(party_colours) <- NULL
  
  party_colours <- c(party_colours, "darkgoldenrod4", "darkred")
  
  plot_df <- lab_props_party %>%
    bind_rows.(labor_props_region, country_props_region) %>% 
    mutate.(
      # {{ main_var }} := str_trunc({{ main_var }}, 20, ellipsis = "."),
      {{ main_var }} := fct_rev(factor({{ main_var }}, 
                                       sort(unique({{ main_var }}))))) %>% 
    mutate(common_group = add_formatting(common_group),
           common_group = fct_relevel(common_group, party_names)
    ) %>% 
    slice_max.(diff_abs, 20)
  
  
  max_diff <- max(plot_df$diff_abs)
  
  int_max_diff <- round(max_diff)
  
  print(party_colours)
  
  print(levels(plot_df$common_group))
  
  ggplot(plot_df) +
    geom_col(
      aes({{ main_var }}, diff, fill = common_group),
      position = "dodge") +
    coord_flip() +
    facet_wrap(~common_group, nrow = 1) +
    labs(x = "", y = "Difference from average") +
    geom_abline(slope = 0, intercept = 0) +
    scale_y_continuous(limits = c(-max_diff, max_diff),
                        labels = c(paste0("-", int_max_diff), 0, int_max_diff),
                        breaks = c(-int_max_diff, 0, int_max_diff)
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      strip.text = element_markdown(),
      axis.line = element_line(colour = "black"),
      legend.position = "none"
    ) +
    basic_add_ons +
    scale_x_discrete() +
    scale_fill_manual(values = party_colours) 
  
}


process_labs <- function(df, 
                         main_var,
                         selection, 
                         party_var = final_alliance,
                         time_var = legislature) {
  
  
  
  if(selection == "all") {
    
    selection <- unique(pull(df, {{ main_var }}))
    
  }
  
  lab_df <- df %>%
    filter.(year > minimum_display_year) %>% 
    summarize.(.by = c(year, {{ time_var }}, {{ party_var }}, {{ main_var }}),
               label_score = sum(score)) %>%
    mutate.(.by = c(year, {{ party_var }}), 
            yr_prop = label_score / sum(label_score)) %>% 
    mutate.(.by = c({{ time_var }}, {{ party_var }}, {{ main_var }}),
            prop = mean(yr_prop)) %>% 
    filter.({{ main_var }} %in% selection) 
  
  lab_df
  
  
}



plot_w_legislature <- function(df, 
                               main_var,
                               party_var = "final_alliance",
                               plot_gov = TRUE) {
  
  
  df %>% 
    ggplot() +
    {if(plot_gov) plot_gov_add_on } +
    geom_line(aes_string("year", "prop", colour = party_var)) +
    facet_wrap(deparse(substitute(main_var))) +
    basic_add_ons +
    labs(x = "", y = "Proportion per year") +
    {if(!is.null(party_var)) scale_colour_manual(values = party_colours[names(party_colours) %in% unique(df$final_alliance)]) } +
    guides(colour = "none") +
    theme(panel.spacing.x = unit(6, "mm"))
  
  
  
  
}