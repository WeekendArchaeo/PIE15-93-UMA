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
# - https://www.stat.berkeley.edu/classes/s133/saving.html
# - https://www.r-bloggers.com/high-resolution-figures-in-r/


library(ggplot2)
library(stringr)

# -------------------
# Pre-activity survey
# -------------------

# Data loading
pre_survey_file_path = 'your_path/'
pre_survey_file_name = 'you_file.csv' # Field delimiter: space
setwd(pre_survey_file_path)
rawdata <- read.table(pre_survey_file_name, header=FALSE, dec=",", quote="\"")
rawdataframe <- data.frame(rawdata)


# * Piecharts *
  
# Question 1
gender_factor <- factor(rawdataframe$V2)
levels(gender_factor) <- c('Female','Male') # The order should be reversed if there is only male students
gender_table <- table(gender_factor)

stackbar_gender <- ggplot(as.data.frame(gender_table), aes(x='',y=as.vector(gender_table),fill=names(gender_table)))+geom_bar(stat='identity',width = 1)+ggtitle('Gender')

piechart_gender <- stackbar_gender+coord_polar('y')+labs(fill='Gender')
piechart_gender_labels_breaks <- cumsum(gender_table)-gender_table/2
piechart_gender_labels <- round(as.vector(prop.table(gender_table))*100,1)
piechart_gender_labels <- gsub(" ", "",paste(as.character(piechart_gender_labels),rep('%',length(gender_table))))
piechart_gender <- piechart_gender+theme(axis.ticks=element_blank(),axis.title=element_blank(),panel.grid = element_blank(),axis.text.x=element_text(color='black'))
piechart_gender <- piechart_gender+scale_y_continuous(breaks=piechart_gender_labels_breaks,labels=piechart_gender_labels)
piechart_gender
piechart_gender_file_name = 'piechart_gender.jpg'
piechart_gender_file_path = paste(pre_survey_file_path,piechart_gender_file_name)
dev.copy(jpeg,piechart_gender_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 2
ages_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V3))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Age")+ggtitle("Age Histogram")
ages_histogram
ages_histogram_file_name = 'ages_histogram.jpg'
ages_histogram_file_path = paste(pre_survey_file_path,ages_histogram_file_name)
dev.copy(jpeg,ages_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

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
piechart_bachelor_file_name = 'piechart_bachelor.jpg'
piechart_bachelor_file_path = paste(pre_survey_file_path,piechart_bachelor_file_name)
dev.copy(jpeg,piechart_bachelor_file_path,width = 6, height = 4, units = 'in', res = 300)
dev.off()

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
piechart_course_file_name = 'piechart_course.jpg'
piechart_course_file_path = paste(pre_survey_file_path,piechart_course_file_name)
dev.copy(jpeg,piechart_course_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

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
piechart_worknow_file_name = 'piechart_worknow.jpg'
piechart_worknow_file_path = paste(pre_survey_file_path,piechart_worknow_file_name)
dev.copy(jpeg,piechart_worknow_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

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
piechart_workbefore_file_name = 'piechart_workbefore.jpg'
piechart_workbefore_file_path = paste(pre_survey_file_path,piechart_workbefore_file_name)
dev.copy(jpeg,piechart_workbefore_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()


# * Histograms *

# Question 8
pre_arduino_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V8))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Previous knowledge about Arduino')
pre_arduino_histogram
pre_arduino_file_name = 'pre_arduino_histogram.jpg'
pre_arduino_file_path = paste(pre_survey_file_path,pre_arduino_file_name)
dev.copy(jpeg,pre_arduino_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 9
otherboards_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V9))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Previous knowledge about other boards')
otherboards_histogram
otherboards_histogram_file_name = 'otherboards_histogram.jpg'
otherboards_histogram_file_path = paste(pre_survey_file_path,otherboards_histogram_file_name)
dev.copy(jpeg,otherboards_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 10
pre_sensact_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V10))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Previous knowledge about sensors/actuators')
pre_sensact_histogram
pre_sensact_histogram_file_name = 'pre_sensact_histogram.jpg'
pre_sensact_histogram_file_path = paste(pre_survey_file_path,pre_sensact_histogram_file_name)
dev.copy(jpeg,pre_sensact_histogram_file_path,width = 5, height = 4, units = 'in', res = 300)
dev.off()

# Question 11
pre_c_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V11))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Previous knowledge about C language')
pre_c_histogram
pre_c_histogram_file_name = 'pre_c_histogram_histogram.jpg'
pre_c_histogram_file_path = paste(pre_survey_file_path,pre_c_histogram_file_name)
dev.copy(jpeg,pre_c_histogram_file_path,width = 5, height = 4, units = 'in', res = 300)
dev.off()



# -------------------
# Post-activity survey
# -------------------

# Data loading
post_survey_file_path = 'your_path/'
post_survey_file_name = 'you_file.csv' # Field delimiter: space
setwd(post_survey_file_path)
rawdata <- read.table(post_survey_file_name, header=FALSE, dec=",", quote="\"")
rawdataframe <- data.frame(rawdata)

# Question 1
programming_diff_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V2))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle("Programming board difficulty")
programming_diff_histogram
programming_diff_histogram_file_name = 'programming_diff_histogram.jpg'
programming_diff_histogram_file_path = paste(post_survey_file_path,programming_diff_histogram_file_name)
dev.copy(jpeg,programming_diff_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 2
ope_diff_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V3))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle("Operating board difficulty")
ope_diff_histogram
ope_diff_histogram_file_name = 'ope_diff_histogram.jpg'
ope_diff_histogram_file_path = paste(post_survey_file_path,ope_diff_histogram_file_name)
dev.copy(jpeg,ope_diff_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 3
labprac_diff_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V4))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle("Lab practices difficulty")
labprac_diff_histogram
labprac_diff_histogram_file_name = 'labprac_diff_histogram.jpg'
labprac_diff_histogram_file_path = paste(post_survey_file_path,labprac_diff_histogram_file_name)
dev.copy(jpeg,labprac_diff_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 6
qmaterial_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V5))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle("Quality of provided lab material")
qmaterial_histogram
qmaterial_histogram_file_name = 'qmaterial_histogram.jpg'
qmaterial_histogram_file_path = paste(post_survey_file_path,qmaterial_histogram_file_name)
dev.copy(jpeg,qmaterial_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 7
teacheratt_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V6))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle("Teacher's attention")
teacheratt_histogram
teacheratt_histogram_file_name = 'teacheratt_histogram.jpg'
teacheratt_histogram_file_path = paste(post_survey_file_path,teacheratt_histogram_file_name)
dev.copy(jpeg,teacheratt_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 8
post_arduino_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V7))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Current knowledge about Arduino')
post_arduino_histogram
post_arduino_histogram_file_name = 'post_arduino_histogram.jpg'
post_arduino_histogram_file_path = paste(post_survey_file_path,post_arduino_histogram_file_name)
dev.copy(jpeg,post_arduino_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 9
post_sensact_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V8))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Current knowledge about sensors')
post_sensact_histogram
post_sensact_histogram_file_name = 'post_sensact_histogram.jpg'
post_sensact_histogram_file_path = paste(post_survey_file_path,post_sensact_histogram_file_name)
dev.copy(jpeg,post_sensact_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 10
post_c_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V9))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=5]")+ggtitle('Current knowledge about C language')
post_c_histogram
post_c_histogram_file_name = 'post_c_histogram.jpg'
post_c_histogram_file_path = paste(post_survey_file_path,post_c_histogram_file_name)
dev.copy(jpeg,post_c_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 11
helpful_factor <- factor(rawdataframe$V10)
levels(helpful_factor) <- c('No','Yes')
helpful_table <- table(helpful_factor)

stackbar_helpful <- ggplot(as.data.frame(helpful_table), aes(x='',y=as.vector(helpful_table),fill=names(helpful_table)))+geom_bar(stat='identity',width = 1)+ggtitle('Were the lab practices helpful for a better understanding of the theory?')

piechart_helpful <- stackbar_helpful+coord_polar('y')+labs(fill='Helpful')
piechart_helpful_labels_breaks <- cumsum(helpful_table)-helpful_table/2
piechart_helpful_labels <- round(as.vector(prop.table(helpful_table))*100,1)
piechart_helpful_labels <- gsub(" ", "",paste(as.character(piechart_helpful_labels),rep('%',length(helpful_table))))
piechart_helpful <- piechart_helpful+theme(axis.ticks=element_blank(),axis.title=element_blank(),panel.grid = element_blank(),axis.text.x=element_text(color='black'))
piechart_helpful <- piechart_helpful+scale_y_continuous(breaks=piechart_helpful_labels_breaks,labels=piechart_helpful_labels)
piechart_helpful
piechart_helpful_file_name = 'piechart_helpful.jpg'
piechart_helpful_file_path = paste(post_survey_file_path,piechart_helpful_file_name)
dev.copy(jpeg,piechart_helpful_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()

# Question 12
globalexp_histogram <- ggplot(data=rawdataframe, aes(rawdataframe$V11))+geom_histogram(binwidth=0.5,colour="black",fill="lightblue")+ylab("Frequency")+xlab("Scale [Min=1, Max=10]")+ggtitle('Global experience')
globalexp_histogram
globalexp_histogram_file_name = 'globalexp_histogram.jpg'
globalexp_histogram_file_path = paste(post_survey_file_path,globalexp_histogram_file_name)
dev.copy(jpeg,globalexp_histogram_file_path,width = 4, height = 4, units = 'in', res = 300)
dev.off()


