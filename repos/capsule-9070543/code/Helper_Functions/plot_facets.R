plot_facets = function(dose_data, xx, yy, group, label){
  ggplot(dose_data, aes_(x = as.name(xx), y = as.name(yy), group = as.name(group)))+
    geom_smooth(method = 'loess', span = 0.5, se=TRUE)+
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1))+
    scale_x_log10(labels = fancy_scientific)+
    xlab(label)+
    ylab("Normalized Cell Number")+
    geom_point()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_wrap(as.name(group))
}