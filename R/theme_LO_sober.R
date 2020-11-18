#' @title Sober theme for ggplot
#'
#' @description Object: Sober theme for ggplot
#' @importFrom ggplot2 element_text element_blank theme ggplot aes geom_bar coord_flip xlim ylim labs guides xlab ylab scale_shape_manual scale_color_manual scale_fill_manual ggtitle geom_abline geom_point geom_errorbar geom_errorbarh
#' @return
#' @export
#'
#' @examples
#'PLOT_MAIN_Text<-ggplot + theme_LO_sober

#Theme
theme_LO_sober <- ggplot2::theme(plot.title = element_text(size = 12, face="bold", hjust = 0.5),
                                 axis.title.x = element_text(size = 10),
                                 axis.title.y = element_text(size = 10),
                                 axis.text.x = element_text(size = 8),
                                 axis.text.y = element_text(size = 8),
                                 panel.background = ggplot2::element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
                                 panel.grid.major.y = ggplot2::element_line(size = 0.025, linetype = 'solid', colour = "grey"),
                                 panel.grid.minor.y = element_blank(),
                                 panel.grid.major.x = element_blank(),
                                 panel.grid.minor.x = element_blank(),
                                 axis.line = ggplot2::element_line(colour = "grey30", size = 0.4, linetype = "solid"))

