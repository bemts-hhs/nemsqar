ems_theme <-   theme(panel.background = element_rect(fill = NA),
                     panel.grid.major = element_line(colour = "#D6E9E1"),
                     panel.grid.minor = element_line(colour = "#D6E9E1"),
                     plot.title = element_text(hjust = 0, color = "#005595", size=14, face="bold"),
                     legend.title = element_blank(),
                     #legend.justification = c("center", "bottom"),
                     legend.position = c(1.05, 0.5),
                     #legend.direction = "horizontal",
                     legend.background = element_rect(fill = "#D6E9E1", 
                                                      size=0.5, 
                                                      linetype="solid", 
                                                      colour = "#c0c0c0"),
                     legend.text = element_text(size=14),
                     axis.text=element_text(size=12),
                     axis.title = element_text(vjust = 1, size=12, face="bold"),
                     axis.ticks = element_blank(),
                     strip.background = element_rect(fill = "#D6E9E1"))




ems_percent_bar_theme <-   theme(panel.background = element_rect(fill = NA),
                     panel.grid.major = element_line(colour = "#D6E9E1"),
                     panel.grid.minor = element_line(colour = "#D6E9E1"),
                     plot.title = element_text(hjust = 0, color = "#005595", size=14, face="bold"),
                     legend.title = element_blank(),
                     #legend.justification = c("center", "bottom"),
                     #legend.position = c(1.05, 0.5),
                     #legend.direction = "horizontal",
                     legend.background = element_rect(fill = "#D6E9E1", 
                                                      size=0.5, 
                                                      linetype="solid", 
                                                      colour = "#c0c0c0"),
                     legend.text = element_text(size=14),
                     axis.text=element_text(size=12),
                     axis.title.x = element_text(vjust = 1, size=12, face="bold"),
                     axis.title.y = element_text(vjust = 1, size=12, face="bold"),
                     axis.ticks = element_blank(),
                     axis.text.y = element_blank(),
                     
                     strip.background = element_rect(fill = "#D6E9E1"))



ems_percent_hbar_theme <-   theme(panel.background = element_rect(fill = NA),
                                 panel.grid.major = element_line(colour = "#D6E9E1"),
                                 panel.grid.minor = element_line(colour = "#D6E9E1"),
                                 plot.title = element_text(hjust = 0, color = "#005595", size=14, face="bold"),
                                 legend.title = element_blank(),
                                 #legend.justification = c("center", "bottom"),
                                 #legend.position = c(1.05, 0.5),
                                 #legend.direction = "horizontal",
                                 legend.background = element_rect(fill = "#D6E9E1", 
                                                                  size=0.5, 
                                                                  linetype="solid", 
                                                                  colour = "#c0c0c0"),
                                 legend.text = element_text(size=14),
                                 
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_text(vjust = 1, size=12, face="bold"),
                                 
                                 
                                 strip.background = element_rect(fill = "#D6E9E1"))