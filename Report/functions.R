grid_arrange_shared_legend <- function(..., n.col = 1) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = "top"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  plt.lst <- lapply(plots, function(x) x + theme(legend.position = "none"))
  plt.lst$ncol <- n.col
  grid.arrange(
    legend,
    do.call(arrangeGrob, plt.lst),
    ncol = 1,
    heights = unit.c(lheight, unit(1, "npc") - lheight))
}

plot.scores <- function(model, scrmat, comps){
  comps <- comps
  comb.mat <- combn(comps, m = 2)
  which.pc <- llply(1:ncol(comb.mat), 
                    function(x) paste('PC', comb.mat[, x], sep = ''))
  expl.var <- llply(which.pc, 
                    function(x) explvar(model)[as.numeric(substr(x, 3, 3))])
  
  scrPlot <- llply(seq_along(which.pc), function(y){
    x <- which.pc[[y]]
    expl <- expl.var[[y]]
    ggplot(scrmat, aes_string(x[1], x[2], fill = 'event', group = 'ID')) + 
      geom_point(shape = 21, size = 3) + 
      scale_fill_brewer(type = 'qual', palette = 'Spectral') +
      theme_bw() +
      geom_vline(xintercept = 0, size = 0.5, linetype = 2, color = 'gray2') +
      geom_hline(yintercept = 0, size = 0.5, linetype = 2, color = 'gray2') +
      theme(legend.position = 'top', legend.direction = 'horizontal') +
      guides(fill = guide_legend(title = 'Events', nrow = 2, 
                                 title.position = 'top', title.hjust = 0.5)) +
      labs(x = paste(x[1], '(', round(expl[1], 2), '%)'), 
           y = paste(x[2], '(', round(expl[2], 2), '%)'))
  })
}