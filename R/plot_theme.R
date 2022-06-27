plot_theme<-function(plot){
  
  plot + 
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      plot.margin = margin(1.5, 2.5, 0.5, 1.75, "cm"),
      plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'), 
      plot.subtitle = element_text(hjust = 0.5, size = 20),
      axis.text = element_text(size = 17, color = 'black', face = 'bold'), 
      axis.text.x=element_text(angle=45,hjust=1, size = 17),
      axis.title=element_text(size=17,face="bold"),
      strip.text = element_text(size = 17, face = 'bold'), 
      panel.grid = element_line(color = 'gray'), 
      legend.position = 'bottom',
      legend.key.width=unit(1,"cm"),
      legend.key.height=unit(0.2,"cm"),
      axis.ticks = element_line(colour = "grey70", size = 0.2),
      panel.grid.major = element_line(colour = "grey70", size = 0.2),
    )
  
}