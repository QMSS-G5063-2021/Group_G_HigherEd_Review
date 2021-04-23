ourtheme <- ggplot2::theme(text = element_text(family = "serif"),
                           # specify as white instead of blank
                           panel.background = element_rect(fill = "white"),
                           legend.position = "bottom",
                           axis.ticks = element_blank(),
                           plot.title = element_text(hjust = 0.5),
                           plot.subtitle = element_text(hjust = 0.5),
                           plot.caption = element_text(hjust = 0))

GreenPal <- RColorBrewer::brewer.pal(7, "Greens")[c(7, 5, 3)]
