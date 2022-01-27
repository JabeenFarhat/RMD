#
#Required libraries
library(Rmisc)
library(ggplot2)

#Read in data.
duration <- read.table("filepath/data.csv", sep = ",", header = T)

head(duration)



#Calculate mean duration of target syllable. Group means by the desired dependent variable(s). Remove NAs. Use the Confidence Interval of 95. 
ci <- summarySE(duration, measurevar = "rel4", groupvars = c("f_type"), na.rm = T, conf.interval = .95)

#Calculate the upper and lower values for plotting error bars.
ci$lower <- ci$rel4 - ci$ci
ci$upper <- ci$rel4 + ci$ci

#Optional: Change column names.
colnames(ci) <- c("focus", "N", "duration", "sd", "se", "ci", "lower", "upper")


#Plot a bar chart. Modify labels on x-axis and y-axis. Specify the text size and face for the axis and the title. Use geom_errorbar() to add errorbars based on Confidence interval.
ggplot(data = ci, aes(x = focus, y = duration)) + geom_bar(position = position_dodge(.9), colour = "black", stat = "identity") + xlab("Focus type") + ylab("Relative duration") + scale_x_discrete(labels = c("Corrective", "Narrow")) + labs(title = "Mean syllable duration", subtitle = "Whiskers indicate 95% Confidence Interval.") + theme(title = element_text(size = 16, face = "bold"), axis.text = element_text(size=24), axis.title = element_text(size=24,face="bold"), legend.text = element_text(size = 24), legend.title = element_text(size=24), axis.title.y=element_text(vjust=1), axis.title.x=element_text(vjust=0.0)) + geom_errorbar(aes(ymin = lower, ymax = upper), width = .1, position = position_dodge(.9))
