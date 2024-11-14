valueTypePlot <- data_values %>%
  mutate(comparisonOutcome = fct_collapse( 
    comparisonOutcome,
    "MAJOR NUMERICAL DISCREPANCY" = c("MAJOR NUMERICAL DISCREPANCY","DECISION_ERROR")),
    valueType = fct_collapse( # combine some value types into higher level categories
      valueType, 
      "Effect sizes" = c("d","r", "pes", "phi"),
      "Test statistics" = c("F", "t", "x2"),
      "Variation/uncertainty measures" = c("sd", "se", "ci"),
      "P-values" = "p",
      "Central tendency measures" = c("mean", "median"),
      "Degrees of freedom" = c("df"),
      "Counts/proportions" = c("n"),
      "Other" = "other"),
    valueType = fct_relevel( # change order of levels (and bars in plot)
      valueType, 
      "Central tendency measures", 
      "Variation/uncertainty measures",
      "P-values",
      "Effect sizes",
      "Test statistics",
      "Counts/proportions",
      "Degrees of freedom",
      "Other"
    )) %>%
  count(valueType, comparisonOutcome) %>% 
  ggplot(aes(x=valueType, y = n, fill = comparisonOutcome)) + 
  geom_col(colour = 'black', width = .8) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(name = 'reproducibility\noutcome', 
                    values = c(green, blue, purple),
                    labels = c("Match", "Minor\ndiscrepancy", "Major\ndiscrepancy")) +
  ylab('count') +
  xlab('\nvalue type') +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_text(colour = 'black'),
        panel.grid.major.x = element_blank(),
        legend.position = 'bottom')