library(ggplot2)
library(ggforce)
library(dplyr)
library(cowplot)


#ex 1----

number_of_arcs<-15

number_x_grid_points<-5
number_y_grid_points<-5

max_pi<-7*pi/8

arcs <- data.frame(
  start = rep(0,number_of_arcs),
  end = seq(0,max_pi,max_pi/number_of_arcs)[-1],
  r = seq(0,1, 1/number_of_arcs)[-1],
  x_centre=2*rep(rep(seq(1,number_x_grid_points),length=number_x_grid_points*number_y_grid_points),
               each=number_of_arcs),
  y_centre=2*rep(rep(seq(1,number_x_grid_points),each=number_y_grid_points),
               each=number_of_arcs)
)

arcs<-arcs%>%
  mutate(start=start+pi,
         end=end+pi)%>%
  bind_rows(arcs)

line_colour<-"white"
background_colour<-"black"

ggplot(arcs) +
  geom_arc(aes(x0 = x_centre,
               y0 = y_centre,
               r = r,
               start = start,
               end = end),
               color=line_colour)+
  xlim(0,2*(number_x_grid_points+1))+
  ylim(0,2*(number_y_grid_points+1))+
  theme(panel.background = element_rect(fill = background_colour, color = background_colour),
        panel.grid.minor=element_line(colour=background_colour),
        panel.grid.major=element_line(colour=background_colour),
        axis.title.x=element_blank (), axis.text.x=element_blank (), axis.ticks.x=element_blank (),
        axis.title.y=element_blank (), axis.text.y=element_blank (), axis.ticks.y=element_blank ())


#ex 2----

number_of_arcs<-8

number_x_grid_points<-3
number_y_grid_points<-3
max_r<-1

max_pi<-12*pi/16

arcs <- data.frame(
  start = rep(0,number_of_arcs),
  end = seq(0,max_pi,max_pi/number_of_arcs)[-1],
  r = seq(0,max_r, 1/number_of_arcs)[-1],
  x_centre=2*rep(rep(seq(1,number_x_grid_points),length=number_x_grid_points*number_y_grid_points),
                 each=number_of_arcs),
  y_centre=2*rep(rep(seq(1,number_x_grid_points),each=number_y_grid_points),
                 each=number_of_arcs)
)

arcs<-arcs%>%
  mutate(start=start+pi,
         end=end+pi)%>%
  bind_rows(arcs)

line_colour<-"grey"
background_colour<-"black"

plot<-arcs%>%
  ggplot() +
  geom_arc(aes(x0 = x_centre,
               y0 = y_centre,
               r=r,
               start = start,
               end = end,
               size = stat(index),
               colour=factor(r)),
           lineend = 'round') +
  scale_radius()+
  scale_color_manual(values=paste0(line_colour,round(seq(100,100/number_of_arcs,-(100/number_of_arcs)),0)))+
  xlim(0,2*(number_x_grid_points+1))+
  ylim(0,2*(number_y_grid_points+1))+
  theme(panel.background = element_rect(fill = background_colour, color = background_colour),
        panel.grid.minor=element_line(colour=background_colour),
        panel.grid.major=element_line(colour=background_colour),
        axis.title.x=element_blank (), axis.text.x=element_blank (), axis.ticks.x=element_blank (),
        axis.title.y=element_blank (), axis.text.y=element_blank (), axis.ticks.y=element_blank (),
        legend.position = "none")

plot

save_plot("circles.png", plot, base_height = 10, base_width = 10)

