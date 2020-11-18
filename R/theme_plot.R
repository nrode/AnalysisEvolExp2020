##
#' Ggplot theme for sober plot
#'
#' @param
#' @param
#'
#' @return
#' @export
#'
#' @examples

#Theme
theme_LO_sobre <- theme(plot.title = element_text( size=12,face="bold",hjust = 0.5),
                        axis.title.x = element_text( size=10),
                        axis.title.y = element_text( size=10),
                        axis.text.x  = element_text( size=8),
                        axis.text.y  =  element_text( size=8),
                        panel.background = element_rect(fill = "white",colour = "white",size = 0.5, linetype = "solid"),
                        panel.grid.major.y = element_line(size = 0.025, linetype = 'solid',colour = "grey"),
                        panel.grid.minor.y = element_blank(),
                        panel.grid.major.x= element_blank(),
                        panel.grid.minor.x= element_blank(),
                        axis.line = element_line(colour = "grey30", size = 0.4, linetype = "solid"))
#Position dodge
pd <- position_dodge(0.3) # move them .05 to the left and right
