#
#Required libraries
library(ggplot2)


wh_boundary <- read.table("filepath/data.csv", sep = ",", header = T)

head(wh_boundary)


#Create a table of boundary tones (counts).
wh <- ftable(wh_boundary$boundary) 
#Get percentage for each boundary tone.
wh_btone <- 100 * prop.table(wh, 1)

#Transform percentages into a dataframe.
tone2 <- as.data.frame((wh_btone))
#Modify column names (optional).
colnames(tone2) <- c("Tone", "Freq")

#Use ggplot to create a barplot to plot percentages.
ggplot(data = tone2, aes(x = Tone, y = Freq)) +  theme_bw() + theme(axis.title.y = element_blank()) + geom_bar(position = 'dodge', stat = 'identity', col = "black") + geom_text(data = tone2, aes(label = paste0(round(Freq,1), "%"), y = Freq + 3), position = position_dodge(width = 1))+  xlab("Tones") + ylab("") + scale_x_discrete(labels = c( "H%", "L%", "LH%")) + theme(title = element_text(size = 10, face = "bold"), axis.text = element_text(size = 24), axis.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20), legend.title = element_text(size = 14), axis.title.y = element_text(vjust = 1), axis.title.x = element_text(vjust = 0.0)) + labs(title = "Sentence final tone of wh-questions in Urdu (Who, Whom, Where)", subtitle = "H% = High, L%: Low, LH%: Rising")

#Find the barplot in the images folder. The image has the same name as this R file.
