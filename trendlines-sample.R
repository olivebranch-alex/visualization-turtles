# Sample script for LOESS trendlines

library(ggplot2)
library(pals) 
library(patchwork)
library(tidyr)

    mesa = read.csv("start_pop_a.csv", skip = 6)
    coffeetable1 = subset (mesa, select = c( rec_rate, starting_turtles, mcon, mpub, ticks, loss_no, kept_no, count.As.with..arg...0., count.As.with..arg...1., count.As.with..arg...2., count.Ss.with..arg...0.,count.Ss.with..arg...1., count.Ss.with..arg...2.))
    colnames(coffeetable1) = c( "rec_rate", "start_pop","mcon", "mpub", "ticks", "loss_no", "kept_no", "A_0", "A_1", "A_2", "S_0", "S_1", "S_2")
    coffeetable1
    
    coffeetable = rbind(coffeetable1) #coffeetable2, coffeetable3_1, coffeetable4, coffeetable5, coffeetable6, coffeetable7, coffeetable8) coffeetable9, coffeetable10)
    coffeetable
    
    coffeetable$popT = (coffeetable$A_2 + coffeetable$A_1 + coffeetable$A_0 + coffeetable$S_1 + coffeetable$S_2 + coffeetable$S_0 )
    coffeetable

draw.popT <- function (rr){
  
  grape=ggplot(data = coffeetable[which(coffeetable$rec_rate==rr),],
               aes(x = ticks, y = popT, group=start_pop, color = as.factor(start_pop), fill = as.factor(start_pop))) + 
    scale_color_manual(name="N (Starting population)", values=parula(7)) + 
    scale_fill_manual(name="N (Starting population)", values=parula(7)) + 
    stat_smooth(se = T, method = "loess", span = 0.2, geom = "smooth", n=100, level=0.95) +
    scale_x_continuous(limits = c(0, 60)) + 
    scale_y_continuous(labels=scales::scientific, limits = c(0,200000)) +
    #geom_smooth(se=T, method="loess", span=0.2 ) +
    #geom_point() +
    theme_bw(base_size=25) +
    labs (y="Total population", x = "Number of generations")
  
  return(grape)
  
}
