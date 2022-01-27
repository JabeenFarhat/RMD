#
#Required libraries
library(dplyr)
library(ggplot2)


#Read in data.
polar_Q <- read.table("filepath/data.csv", sep = ",", header = T)



#The column number for the variable where calculation should start.
firstrealcolumn <- 30

#Last column number to stop calculation.
last_IVcolumn = firstrealcolumn - 1

#The columns to be used for plotting.
columns.to.use <- firstrealcolumn:ncol(polar_Q)

#Loop over the target columns.
for(i in 1:length(columns.to.use)) {
  data.temp <- polar_Q[, c(1:last_IVcolumn, columns.to.use[i])]  
  names(data.temp)[firstrealcolumn] <- "dv"
  
  Summary.df <- data.temp %>%     #%>% means "and then" 
    group_by(prominence)%>%       #The grouping variable
    summarise(
      Observations = n(),
      Observations2 = sum(!is.na(dv)),
      Mean = mean(dv, na.rm = TRUE), 
      SD = sd(dv, na.rm = TRUE), 
      SE = SD/sqrt(Observations2)
    ) 
  
  Summary.df$time <- i
  if(i == 1) {combined <- Summary.df}
  if(i > 1) {combined <- rbind(combined, Summary.df)}
}


#Plot data. geom_vline() adds vertical lines to the plot. annotate() is used to add text. It takes arguments for coordinates of the text and the label and font for the text. geom_ribbon() draws the band around the lineplot. Its minimum and maximum values are calculated using mean and standard error. theme() is used to adjust text size and face of labels on x-axis, y-axis, and title of the plot.  
ggplot(data = combined, aes(x = time, y = Mean)) + geom_vline(xintercept = 2) + geom_vline(xintercept = 4) + geom_vline(xintercept = 6) + geom_vline(xintercept = 8) + annotate(geom = "text", x = 1.2, y = 200, label = "Q particle", fontface = "bold") + annotate(geom = "text", x = 3.2, y = 200, label = "Subject", fontface = "bold", color = "red") + annotate(geom = "text", x = 5, y = 200, label = "Object", fontface = "bold") + annotate(geom = "text", x = 7, y = 200, label = "Object", fontface = "bold") + annotate(geom = "text", x = 9.3, y = 200, label = "Verb", fontface = "bold") + geom_ribbon(aes(ymin = Mean-SE, ymax = Mean + SE), fill = "darkgray", alpha = .5, colour = FALSE) +
  geom_line(size = 1.5) + xlab("Grammatical Role") + ylab("Fundamental Frequency (Hz)") + labs(title = "Fundamental Frequency of Urdu Yes/no Questions", subtitle = "Sentence initial question particle (`Kya' what). Subject is questioned.") + theme(axis.title.y = element_text(size = 20, face = "bold", vjust = 1.5), axis.title.x = element_text(size = 18, face = "bold", vjust = -0.5), axis.text = element_text(size = 18), plot.title = element_text(size = 18, face = "bold", vjust = 1.5)) + scale_color_manual(values = "black") 














