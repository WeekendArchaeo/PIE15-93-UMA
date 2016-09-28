# --------------------------------------
#           PIE 15-93
# 
#   Anonymous surveys for students
#
# This work has been partially funded by
# the Teaching Innovation 2015-2017 Plan
# at Universidad of Málaga
# --------------------------------------


# This code is based in ideas and solutions provided by these sources. Thanks a lot for sharing!!
# - http://www.statmethods.net/graphs/pie.html
# - http://stackoverflow.com/questions/26788049/plot-table-objects-with-ggplot
# - http://mathematicalcoffee.blogspot.com.es/2014/06/ggpie-pie-graphs-in-ggplot2.html
# - http://docs.ggplot2.org/current/scale_continuous.html
# - http://stackoverflow.com/questions/11936339/in-r-how-do-i-replace-text-within-a-string


library(ggplot2)
library(stringr)

# -------------------
# Pre-activity survey
# -------------------

# Data loading
rawdata <- read.table("mypredatafile.csv", header=FALSE, dec=",", quote="\"")
rawdataframe <- data.frame(rawdata)


# * Piecharts *
  

# Question 1
gender_factor <- factor(rawdataframe$V2)
levels(gender_factor) <- c('Female','Male')
gender_table <- table(gender_factor)

stackbar_gender <- ggplot(as.data.frame(gender_table), aes(x='',y=as.vector(gender_table),fill=names(gender_table)))+geom_bar(stat='identity',width = 1)+ggtitle('Gender')

piechart_gender <- stackbar_gender+coord_polar('y')+labs(fill='Gender')
piechart_gender_labels_breaks <- cumsum(gender_table)-gender_table/2
piechart_gender_labels <- round(as.vector(prop.table(gender_table))*100,1)
piechart_gender_labels <- gsub(" ", "",paste(as.character(piechart_gender_labels),rep('%',length(gender_table))))
piechart_gender <- piechart_gender+theme(axis.ticks=element_blank(),axis.title=element_blank(),panel.grid = element_blank(),axis.text.x=element_text(color='black'))
piechart_gender <- piechart_gender+scale_y_continuous(breaks=piechart_gender_labels_breaks,labels=piechart_gender_labels)
piechart_gender


# Question 2
ages_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V3))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Age")+ggtitle("Ages Histogram")
ages_histogram


# Question 3
bachelor_factor <- factor(rawdataframe$V4)
levels(bachelor_factor) <- c('BSc in Computer Science','BSc in Computer Engineering','BSc in Software Engineering')
bachelor_table <- table(bachelor_factor)

stackbar_bachelor <- ggplot(as.data.frame(bachelor_table), aes(x='',y=as.vector(bachelor_table),fill=names(bachelor_table)))+geom_bar(stat='identity',width = 1)+ggtitle('Bachelor Program')

piechart_bachelor <- stackbar_bachelor+coord_polar('y')+labs(fill='BSc')
piechart_bachelor_labels_breaks <- cumsum(bachelor_table)-bachelor_table/2
piechart_bachelor_labels <- round(as.vector(prop.table(bachelor_table))*100,1)
piechart_bachelor_labels <- gsub(" ", "",paste(as.character(piechart_bachelor_labels),rep('%',length(bachelor_table))))
piechart_bachelor <- piechart_bachelor+theme(axis.ticks=element_blank(),axis.title=element_blank(),panel.grid = element_blank(),axis.text.x=element_text(color='black'))
piechart_bachelor <- piechart_bachelor+scale_y_continuous(breaks=piechart_bachelor_labels_breaks,labels=piechart_bachelor_labels)
piechart_bachelor


# Question 4
course_factor <- factor(rawdataframe$V5)
levels(course_factor) <- c('Third','Fourth')
course_table <- table(course_factor)

stackbar_course <- ggplot(as.data.frame(course_table), aes(x='',y=as.vector(course_table),fill=names(course_table)))+geom_bar(stat='identity',width = 1)+ggtitle('Year')

piechart_course <- stackbar_course+coord_polar('y')+labs(fill='Year')
piechart_course_labels_breaks <- cumsum(course_table)-course_table/2
piechart_course_labels <- round(as.vector(prop.table(course_table))*100,1)
piechart_course_labels <- gsub(" ", "",paste(as.character(piechart_course_labels),rep('%',length(course_table))))
piechart_course <- piechart_course+theme(axis.ticks=element_blank(),axis.title=element_blank(),panel.grid = element_blank(),axis.text.x=element_text(color='black'))
piechart_course <- piechart_course+scale_y_continuous(breaks=piechart_course_labels_breaks,labels=piechart_course_labels)
piechart_course


# Question 5
worknow_factor <- factor(rawdataframe$V6)
levels(worknow_factor) <- c('No','Yes')
worknow_table <- table(worknow_factor)

stackbar_worknow <- ggplot(as.data.frame(worknow_table), aes(x='',y=as.vector(worknow_table),fill=names(worknow_table)))+geom_bar(stat='identity',width = 1)+ggtitle('Are you currently employed?')

piechart_worknow <- stackbar_worknow+coord_polar('y')+labs(fill='Currently working')
piechart_worknow_labels_breaks <- cumsum(worknow_table)-worknow_table/2
piechart_worknow_labels <- round(as.vector(prop.table(worknow_table))*100,1)
piechart_worknow_labels <- gsub(" ", "",paste(as.character(piechart_worknow_labels),rep('%',length(worknow_table))))
piechart_worknow <- piechart_worknow+theme(axis.ticks=element_blank(),axis.title=element_blank(),panel.grid = element_blank(),axis.text.x=element_text(color='black'))
piechart_worknow <- piechart_worknow+scale_y_continuous(breaks=piechart_worknow_labels_breaks,labels=piechart_worknow_labels)
piechart_worknow


# Question 6
workbefore_factor <- factor(rawdataframe$V7)
levels(workbefore_factor) <- c('No','Yes')
workbefore_table <- table(workbefore_factor)

stackbar_workbefore <- ggplot(as.data.frame(workbefore_table), aes(x='',y=as.vector(workbefore_table),fill=names(workbefore_table)))+geom_bar(stat='identity',width = 1)+ggtitle('Have you been previously employed?')

piechart_workbefore <- stackbar_workbefore+coord_polar('y')+labs(fill='Previous employment')
piechart_workbefore_labels_breaks <- cumsum(workbefore_table)-workbefore_table/2
piechart_workbefore_labels <- round(as.vector(prop.table(workbefore_table))*100,1)
piechart_workbefore_labels <- gsub(" ", "",paste(as.character(piechart_workbefore_labels),rep('%',length(workbefore_table))))
piechart_workbefore <- piechart_workbefore+theme(axis.ticks=element_blank(),axis.title=element_blank(),panel.grid = element_blank(),axis.text.x=element_text(color='black'))
piechart_workbefore <- piechart_workbefore+scale_y_continuous(breaks=piechart_workbefore_labels_breaks,labels=piechart_workbefore_labels)
piechart_workbefore


# * Histograms *

# Question 8
arduino_histogram <- ggplot(data=rawdataframe, aes(rawdataframe[8]))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Previous knowledge about Arduino')
arduino_histogram

# Question 9
otherboards_histogram <- ggplot(data=rawdataframe, aes(rawdataframe[9]))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Previous knowledge about other boards')
otherboards_histogram

# Question 10
sensact_histogram <- ggplot(data=rawdataframe, aes(rawdataframe[10]))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Previous knowledge about sensors/actuators')
sensact_histogram

# Question 11
c_histogram <- ggplot(data=rawdataframe, aes(rawdataframe[11]))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Previous knowledge about C language')
c_histogram


# -------------------
# Post-activity survey
# -------------------

# Data loading
rawdata <- read.table("mypostdatafile.csv", header=FALSE, dec=",", quote="\"")
rawdataframe <- data.frame(rawdata)

arduino_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V1))+geom_histogram(binwidth=0.5,colour="black",fill="lightgreen")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Current knowledge about Arduino')
arduino_histogram

sensact_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V1))+geom_histogram(binwidth=0.5,colour="black",fill="lightgreen")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Current knowledge about sensors/actuators')
sensact_histogram

c_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V2))+geom_histogram(binwidth=0.5,colour="black",fill="lightgreen")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Current knowledge about C language')
c_histogram
