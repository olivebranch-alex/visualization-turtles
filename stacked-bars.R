# Sample script for making of stacked bar graphs

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

    coffeetable$RecombRes = (coffeetable$A_2 + coffeetable$A_1)
    coffeetable$RecombSus = (coffeetable$A_0)
    coffeetable$NrecombRes = (coffeetable$S_2 + coffeetable$S_1)
    coffeetable$NrecombSus = (coffeetable$S_0)
    
    
    
    coffeetable_long <- pivot_longer(coffeetable, cols = c(RecombRes, RecombSus, NrecombRes, NrecombSus), names_to = "alltypes", values_to = "count_alltypes" ) 
    coffeetable_long
    
    coffeetable_long <- coffeetable %>%
      pivot_longer(cols = c(RecombRes, RecombSus, NrecombRes, NrecombSus),  # Specify the columns to pivot
                   names_to = "alltypes",  # The new column name for types
                   values_to = "count_alltypes")
    
    
    draw.alltypes <- function (mc, sp){
      
      grape=ggplot(data = coffeetable_long[which(coffeetable_long$mcon==mc & coffeetable_long$start_pop==sp),],
                   aes(x = ticks, y = count_alltypes/100 , fill = alltypes)) + 
        geom_bar(stat = "identity") + scale_fill_manual (values = c("RecombRes" = "#97B0A7",
                                                                   "RecombSus" = "#C7DAD8",  
                                                                    "NrecombRes" = "#FFA1D4", 
                                                                    "NrecombSus" = "#EFC8ED"), 
                                                         labels = c("RecombRes" = "Recombiner, resistant",
                                                                    "RecombSus" = "Recombiner, susceptible",
                                                                   "NrecombRes" = "Non-recombiner, resistant", 
                                                                    "NrecombSus" = "Non-recombiner, susceptible"))  + 
        # Setup max and min values for the x and y axes
        scale_x_continuous(limits = c(0, 60)) + 
        scale_y_continuous(labels=scales::scientific, limits = c(0, 250000)) +
        #ylim(0, 250000) +
     #  scale_y_continuous(limits = c(0, 25000000)) + 
        labs (y="Number of turtles", x = "Number of generations", fill = "Classification of bacteria") +
        theme_bw(base_size=15) 
      
      return(grape)
      
    }
