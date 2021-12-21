
jinchu_colours <- c("M. China" = "orange",
                    "India" = "darkgreen",
                    "Japan" = "darkorchid3",
                    "South Korea" = "darksalmon",
                    "Taiwan" = "deeppink",
                    "Australia" = "darkgreen",
                    "Indonesia" = "red",
                    "Colombia" = "gold3",
                    "United States" = "darkblue",
                    "Canada" = "darkorchid",
                    "Other" = "grey70",
                    "Europe" = "darkblue",
                    "M. China" = "orange",
                    "(former) USSR" = "darkred",
                    "Russia" = "darkred")




ct_names <- c("People's Republic of China" = "M. China",
              "Korea" = "South Korea",
              # "China (P.R. of China and Hong Kong, China)" = "China",
              "Former Soviet Union (if no detail)" = "(former) USSR",
              "Kazakhstan" = "(former) USSR",
              "Uzbekistan" = "(former) USSR",
              "Turkmenistan" = "(former) USSR",
              "Kyrgyzstan"= "(former) USSR",
              "Russia" = "(former) USSR",
              "Chinese Taipei" = "Taiwan",
              "United Kingdom" = "Europe",
              "Germany" = "Europe",
              "Netherlands" = "Europe",
              "Italy" = "Europe",
              "France" = "Europe",
              "Spain" = "Europe",
              "Belgium" = "Europe",
              "Taiwan / Chinese Taipei" = "Taiwan",
              "People's Republic of China" = "M. China")



top_iea_value <- function(df, N = 7) {
  
  top_ausexports <- df %>% 
    summarize.(.by = simple_country, total = abs(sum(Value, na.rm = T))) %>% 
    slice_max.(total, n = N) 
  
}




filt_ieacoal_products <- function(flow) {
  
  ieacoal %>% 
    filter.(FLOW == flow, PRODUCT %in% c("MHARD", "MBROWN")) %>% 
    mutate.(simple_country = dplyr::recode(Country, !!!ct_names)) %>% 
    filter.(!str_detect(simple_country, "World|European|OECD|Total|IEA|,|:")) %>% 
    filter.(TIME > 1963)
  
}



basic_jinchukou <- function(df, group_by = c("TIME", "simple_country")) {
  
  plot_df <- df %>% 
    summarize.(.by = all_of(group_by), tonnes = abs(sum(Value, na.rm = T))/1000) %>% 
    mutate.(simple_country = fct_rev(fct_reorder(simple_country, tonnes, max)))
  
  jinchu_colours <- jinchu_colours[names(jinchu_colours) %in% plot_df$simple_country]
  
  jinchu_colours <- jinchu_colours[order(match(names(jinchu_colours), 
                                               levels(plot_df$simple_country)))]
  
  ggplot(plot_df) +
    geom_line(aes(TIME, tonnes, colour = simple_country)) +
    scale_colour_manual(values = jinchu_colours) 
  
}


imports_add_on <- list(labs(x = "", y = "", colour = ""),
                       theme(text = element_text(family="Serif")),
                       scale_x_continuous(breaks = c(1980, 2000, 2020)),
                       scale_y_continuous(labels = label_number(accuracy = 1)))

