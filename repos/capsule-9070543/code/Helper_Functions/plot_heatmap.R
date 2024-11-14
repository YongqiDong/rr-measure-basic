plot_heatmap = function (plot_data, thresh, ...)
  {
  require(pheatmap)
  if(thresh == FALSE){
    min_up = min(plot_data)
    max_up = max(plot_data)
    ncols_up = 20
    ncols_down = 20
    rc1 = colorRampPalette(colors = c("#00a4ff","black"), space = "Lab")(ncols_up)
    rc2 = colorRampPalette(colors = c("black","#ff0000"), space = "Lab")(ncols_down)
    rampcols = c(rc1[1], rc1, rc2, rc2[ncols_up])
    rb1 = seq(min_up, 0, length.out = ncols_down + 1)
    rb2 = seq(0, max_up, length.out = ncols_up + 1)[-1]
    rampbreaks = c(rb1, rb2)
    pheatmap(plot_data, color=rampcols, breaks=rampbreaks, scale="none", border_color = NA, ...)
  }
  else{
    min_up = min(plot_data)
    max_up = max(plot_data)
    ncols_up = 20
    ncols_down = 20
    rc1 = colorRampPalette(colors = c("#00a4ff","#D1E5F0"), space = "Lab")(ncols_up)
    rc2 = colorRampPalette(colors = c("#FDDBC7","#ff0000"), space = "Lab")(ncols_down)
    rc3 = colorRampPalette(colors = c("#D1E5F0", "black", "#FDDBC7"),space = "Lab")(22)
    rc3 = rc3[2:(length(rc3)-1)]
    rampcols = c(rc1[1], rc1, rc3, rc2, rc2[ncols_up])
    rb1 = seq(min_up, -thresh, length.out = ncols_down + 1)
    rb2 = seq(thresh, max_up, length.out = ncols_up + 1)[-1]
    rb3 = seq(-1*(thresh-0.01), thresh+0.01, length.out = 21)
    rampbreaks = c(rb1, rb3, rb2)
    
    pheatmap(plot_data, color=rampcols, breaks=rampbreaks, scale="none", border_color = NA, ...)
  }
}
