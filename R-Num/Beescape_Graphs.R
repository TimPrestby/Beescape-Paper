#Load packages 
packages = c("ggplot2", "ggpubr", "reshape2", "RColorBrewer","extrafont","svglite","extrafont","plyr")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

lapply(packages, require, character.only = TRUE)

#Load fonts
#font_import(paths = "C:/Windows/Fonts",prompt = F)

loadfonts(device = "win", quiet = TRUE)
windowsFonts("trebuchet ms" = windowsFont("trebuchet ms"))

#load datset

data.full <- read.csv("Beescape_R.csv")

#create legend

"sa" = rgb(94, 60, 153, maxColorValue=255)
"ag" = rgb(178, 171, 210, maxColorValue=255)
"n" = rgb(247, 247, 247, maxColorValue=255)
"di"= rgb(253, 184, 99, maxColorValue=255)
"sd" = rgb(230, 97, 1, maxColorValue=255)


###############################################################################
###QUESTION 1###
##############################################################################

#Filter out data to get columns for question 1 and its sub-parts

data <- data.full[,7:14]

##Convert the qualitative ordinal data into ordinal quantitative data 

data <- ifelse(data == "Strongly Disagree", 1, ifelse(data == "Disagree", 2, ifelse(data == "Neither Agree nor Disagree", 3, ifelse(data == "Agree", 4, ifelse(data == "Strongly Agree", 5, NA)))))

#Set the columns names of 'data' by creating a vector 
colnames(data) <- c("Beekeeping is my primary source of income", "Beekeeping is my secondary source of income", "Beekeeping is my hobby", "I started beekeeping because I enjoy observing bees", "I started beekeeping to connect with nature", "I started beekeeping for the pollination services", "I started beekeeping to help the Earth", "I actively manage my bees")

#reorganize the columns  
data <- data[,c(1,2,6,7,4,5,3,8)]

##Convert the data to a choice:response format that includes all responses 
data.melt <- melt(data)
data.melt <- data.melt[,2:3]
colnames(data.melt) <- c("choice", "response")
data.melt$response <- as.factor(data.melt$response)
df = data.melt
df = count(df,c('choice','response'))


# Get percentages 
df = ddply(df, .(choice), transform, percent = freq/sum(freq) * 100)
df$label = paste0(sprintf("%.0f", df$percent), "%")

##colorF = c("1"="white", "2"= "black","3"="black", "4"="black", "5"="white")

a = ggplot(data=subset(df, !is.na(response)), aes(x = factor(choice), y = freq, fill = response)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = .7) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank(),
    text = element_text(family = "trebuchet ms")) +
  labs(x = "", y = "Count", colour = "") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
  theme(legend.title = element_blank(), plot.margin = unit(c(.5, 0, .5, .5), "cm")) +
  scale_fill_manual(values = c(sd,di,n,ag,sa),labels =
    c("Strongly Disagree", "Disagree", "Neither Agree Nor   Disagree", "Agree", "Strongly Agree"), drop = FALSE) +
  geom_text(aes(label = label, color=response, family = "trebuchet ms", family = "trebuchet ms"),position = position_stack(vjust = 0.5, reverse = TRUE), size = 2) +
  scale_color_manual(values = c("white","black","black","black","white"))+ 
  scale_x_discrete(position = "top")

# #Create sideways 100% stacked bar chart based on counts (response as a factor to categorize)
# a <- ggplot(data=subset(data.melt, !is.na(response)), aes(x = choice, y = response)) +
#   geom_bar(aes(fill=response), position = position_stack(reverse = TRUE)) +
#   coord_flip() +
#   theme_bw(base_size = 12) + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank(), text = element_text(family = "trebuchet ms")) +
#   labs(x = "", y = "Count", colour = "") +
#   scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
#   theme(legend.title = element_blank(), plot.margin = unit(c(.5, 0, .5, .5), "cm")) +
#   scale_fill_manual(values = c(sd,di,n,ag,sa), labels = c("Strongly Disagree", "Disagree", "Neither Agree Nor   Disagree", "Agree", "Strongly Agree"), drop = FALSE) +
#   geom_text(label=label, position = position_stack(vjust = 0.5), size = 2) +
#   scale_x_discrete(position = "top")


#Convert response to numeric to allow for plotting 
data.melt$response <- as.numeric(data.melt$response)

b <- ggplot(data=subset(data.melt, !is.na(response)), aes(x = choice, y = response)) +
  geom_boxplot(alpha=0.7, outlier.shape = NA) +
  stat_summary(geom = "crossbar", fun.y = "mean", color = "gray", width = .75, fatten = 2) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_text(hjust = 0.5, size = 6), text = element_text(family = "trebuchet ms"),
        plot.margin = unit(c(.5, .5, .5, 0), "cm")) +
  labs(x = "", y = "Score Distribution ", colour = "") +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(1,5))


#Create arrangement of figures with the legend on the right 
c <- ggarrange(a,b, ncol = 2, nrow = 1, widths = c(2.25,2), heights = c(2,2), legend = "none", common.legend=TRUE)

ggsave(file="q1.svg", plot=c, width=8, height=3)

###############################################################################
###QUESTION 2###
##############################################################################

#Filter out data to get columns for question 2 and its sub-parts

data <- data.full[,15:19]

data <- ifelse(data == "Strongly Disagree", 1, ifelse(data == "Disagree", 2, ifelse(data == "Neither Agree nor Disagree", 3, ifelse(data == "Agree", 4, ifelse(data == "Strongly Agree", 5, NA)))))

colnames(data) <- c("I understand Nesting", "I understand Insecticide", "I understand Spring Floral", "I understand Summer Floral", "I understand Fall Floral")

data <- data[,c(1,2,4,5,3)]

##Convert the data to a choice:response format that includes all responses 
data.melt <- melt(data)
data.melt <- data.melt[,2:3]
colnames(data.melt) <- c("choice", "response")
data.melt$response <- as.factor(data.melt$response)
df = data.melt
df = count(df,c('choice','response'))


# Get percentages 
df = ddply(df, .(choice), transform, percent = freq/sum(freq) * 100)
df$label = paste0(sprintf("%.0f", df$percent), "%")

##colorF = c("1"="white", "2"= "black","3"="black", "4"="black", "5"="white")

a = ggplot(data=subset(df, !is.na(response)), aes(x = factor(choice), y = freq, fill = response)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = .7) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank(),
        text = element_text(family = "trebuchet ms")) +
  labs(x = "", y = "Count", colour = "") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
  theme(legend.title = element_blank(), plot.margin = unit(c(.5, 0, .5, .5), "cm")) +
  scale_fill_manual(values = c(sd,di,n,ag,sa),labels =
                      c("Strongly Disagree", "Disagree", "Neither Agree Nor   Disagree", "Agree", "Strongly Agree"), drop = FALSE) +
  geom_text(aes(label = label, color=response, family = "trebuchet ms"),position = position_stack(vjust = 0.5, reverse = TRUE), size = 2) +
  scale_color_manual(values = c("white","black","black","black","white"))+ 
  scale_x_discrete(position = "top")

#Convert response to numeric to allow for plotting 
data.melt$response <- as.numeric(data.melt$response)

b <- ggplot(data=subset(data.melt, !is.na(response)), aes(x = choice, y = response)) +
  geom_boxplot(alpha=0.7, outlier.shape = NA) +
  stat_summary(geom = "crossbar", fun.y = "mean", color = "gray", width = .75, fatten = 2) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_text(hjust = 0.5, size = 8), text = element_text(family = "trebuchet ms"),
        plot.margin = unit(c(.5, .5, .5, 0), "cm")) +
  labs(x = "", y = "Score Distribution ", colour = "") +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(1,5))


#Create arrangement of figures with the legend on the right 
c <- ggarrange(a,b, ncol = 2, nrow = 1, widths = c(2.56,4), heights = c(2,2), legend = "none", common.legend=TRUE)
ggsave(file="q2.svg", plot=c, width=15, height=3)

###############################################################################
###QUESTION 3###
##############################################################################


#Filter out data to get columns for question 3 and its sub-parts

data <- data.full[,20:24]

data <- ifelse(data == 1, 1, ifelse(data == 2, 2, ifelse(data == 3, 3, ifelse(data == 4, 4, ifelse(data == 5, 5, NA)))))

colnames(data) <- c("Nesting", "Insecticide", "Spring Floral", "Summer Floral", "Fall Floral")

data <- data[,c(1,5,4,2,3)] 

##Convert the data to a choice:response format that includes all responses 
data.melt <- melt(data)
data.melt <- data.melt[,2:3]
colnames(data.melt) <- c("choice", "response")
data.melt$response <- as.factor(data.melt$response)
df = data.melt
df = count(df,c('choice','response'))


# Get percentages 
df = ddply(df, .(choice), transform, percent = freq/sum(freq) * 100)
df$label = paste0(sprintf("%.0f", df$percent), "%")

a = ggplot(data=subset(df, !is.na(response)), aes(x = factor(choice), y = freq, fill = response)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = .7) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank(),
        text = element_text(family = "trebuchet ms")) +
  labs(x = "", y = "Count", colour = "") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
  theme(legend.title = element_blank(), plot.margin = unit(c(.5, 0, .5, .5), "cm")) +
  scale_fill_brewer(palette="Blues", direction = -1,labels = c("First Choice", "Second Choice", "Third Choice", "Fourth Choice", "Fifth Choice"))+
  geom_text(aes(label = label, color=response, family = "trebuchet ms"),position = position_stack(vjust = 0.5, reverse = TRUE), size = 2) +
  scale_color_manual(values = c("white","white","black","black","black"))+ 
  scale_x_discrete(position = "top")

#Convert response to numeric to allow for plotting 
data.melt$response <- as.numeric(data.melt$response)

b <- ggplot(data=subset(data.melt, !is.na(response)), aes(x = choice, y = response)) +
  geom_boxplot(alpha=0.7, outlier.shape = NA) +
  stat_summary(geom = "crossbar", fun.y = "mean", color = "gray", width = .75, fatten = 2) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_text(hjust = 0.5, size = 8), text = element_text(family = "trebuchet ms"),
        plot.margin = unit(c(.5, .5, .5, 0), "cm")) +
  labs(x = "", y = "Score Distribution ", colour = "") +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(1,5))


#Create arrangement of figures with the legend on the right 
c <- ggarrange(a,b, ncol = 2, nrow = 1, widths = c(2.56,4), heights = c(2,2), legend = "none", common.legend=TRUE)

ggsave(file="q3.svg", plot=c, width=15, height=3)


###############################################################################
###QUESTION 4###
##############################################################################

#Filter out data to get columns for question 4 and its sub-parts

data <- data.full[,25:30]

data <- ifelse(data == "Strongly disagree", 1, ifelse(data == "Disagree", 2, ifelse(data == "Neither agree nor disagree", 3, ifelse(data == "Agree", 4, ifelse(data == "Strongly agree", 5, NA)))))

colnames(data) <- c("The scores in Beescape match my past experiences", "Beescape helped me understand the quality of my landscape for bees", "Beescape provides information that is relevant to me", "Beescape helped me connect with scientists at Penn State", "Beescape would be helpful for planning the best place to put my apiary", "Beescape would help me meet other beekepers")

data <- data[,c(6,4,1,3,2,5)]

##Convert the data to a choice:response format that includes all responses 
data.melt <- melt(data)
data.melt <- data.melt[,2:3]
colnames(data.melt) <- c("choice", "response")
data.melt$response <- as.factor(data.melt$response)
df = data.melt
df = count(df,c('choice','response'))


# Get percentages 
df = ddply(df, .(choice), transform, percent = freq/sum(freq) * 100)
df$label = paste0(sprintf("%.0f", df$percent), "%")

##colorF = c("1"="white", "2"= "black","3"="black", "4"="black", "5"="white")

a = ggplot(data=subset(df, !is.na(response)), aes(x = factor(choice), y = freq, fill = response)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = .7) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank(),
        text = element_text(family = "trebuchet ms")) +
  labs(x = "", y = "Count", colour = "") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
  theme(legend.title = element_blank(), plot.margin = unit(c(.5, 0, .5, .5), "cm")) +
  scale_fill_manual(values = c(sd,di,n,ag,sa),labels =
                      c("Strongly Disagree", "Disagree", "Neither Agree Nor   Disagree", "Agree", "Strongly Agree"), drop = FALSE) +
  geom_text(aes(label = label, color=response, family = "trebuchet ms"),position = position_stack(vjust = 0.5, reverse = TRUE), size = 2) +
  scale_color_manual(values = c("white","black","black","black","white"))+ 
  scale_x_discrete(position = "top")

#Convert response to numeric to allow for plotting 
data.melt$response <- as.numeric(data.melt$response)

b <- ggplot(data=subset(data.melt, !is.na(response)), aes(x = choice, y = response)) +
  geom_boxplot(alpha=0.7, outlier.shape = NA) +
  stat_summary(geom = "crossbar", fun.y = "mean", color = "gray", width = .75, fatten = 2) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_text(hjust = 0.5, size = 8), text = element_text(family = "trebuchet ms"),
        plot.margin = unit(c(.5, .5, .5, 0), "cm")) +
  labs(x = "", y = "Score Distribution ", colour = "") +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(1,5))


#Create arrangement of figures with the legend on the right 
c <- ggarrange(a,b, ncol = 2, nrow = 1, widths = c(2.56,4), heights = c(2,2), legend = "none", common.legend=TRUE)

ggsave(file="q4.svg", plot=c, width=15, height=3)


###############################################################################
###QUESTION 5###
##############################################################################

#Filter out data to get columns for question 5 and its sub-parts

data <- data.full[,31:35]

data <- ifelse(data == "Strongly disagree", 1, ifelse(data == "Disagree", 2, ifelse(data == "Neither agree nor disagree", 3, ifelse(data == "Agree", 4, ifelse(data == "Strongly agree", 5, NA)))))

colnames(data) <- c("I am comfortable sharing the location of my apiary with scientists", "I am comfortable sharing the location of my apiary with other beekeepers", "I am comfortable sharing the location of my apiary with the general public", "I support a publicly accessible map of all apiaries contributing to Beescape", "I am concerned with potential misuse of information")

data <- data[,c(5,3,4,2,1)] 

##Convert the data to a choice:response format that includes all responses 
data.melt <- melt(data)
data.melt <- data.melt[,2:3]
colnames(data.melt) <- c("choice", "response")
data.melt$response <- as.factor(data.melt$response)
df = data.melt
df = count(df,c('choice','response'))


# Get percentages 
df = ddply(df, .(choice), transform, percent = freq/sum(freq) * 100)
df$label = paste0(sprintf("%.0f", df$percent), "%")

##colorF = c("1"="white", "2"= "black","3"="black", "4"="black", "5"="white")

a = ggplot(data=subset(df, !is.na(response)), aes(x = factor(choice), y = freq, fill = response)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = .7) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank(),
        text = element_text(family = "trebuchet ms")) +
  labs(x = "", y = "Count", colour = "") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
  theme(legend.title = element_blank(), plot.margin = unit(c(.5, 0, .5, .5), "cm")) +
  scale_fill_manual(values = c(sd,di,n,ag,sa),labels =
                      c("Strongly Disagree", "Disagree", "Neither Agree Nor   Disagree", "Agree", "Strongly Agree"), drop = FALSE) +
  geom_text(aes(label = label, color=response, family = "trebuchet ms"),position = position_stack(vjust = 0.5, reverse = TRUE), size = 2) +
  scale_color_manual(values = c("white","black","black","black","white"))+ 
  scale_x_discrete(position = "top")

#Convert response to numeric to allow for plotting 
data.melt$response <- as.numeric(data.melt$response)

b <- ggplot(data=subset(data.melt, !is.na(response)), aes(x = choice, y = response)) +
  geom_boxplot(alpha=0.7, outlier.shape = NA) +
  stat_summary(geom = "crossbar", fun.y = "mean", color = "gray", width = .75, fatten = 2) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_text(hjust = 0.5, size = 8), text = element_text(family = "trebuchet ms"),
        plot.margin = unit(c(.5, .5, .5, 0), "cm")) +
  labs(x = "", y = "Score Distribution ", colour = "") +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(1,5))


#Create arrangement of figures with the legend on the right 
c <- ggarrange(a,b, ncol = 2, nrow = 1, widths = c(2.56,4), heights = c(2,2), legend = "none", common.legend=TRUE)

ggsave(file="q5.svg", plot=c, width=15, height=3)

###############################################################################
###QUESTION 6###
##############################################################################

data <- data.full[,36:44]

data <- ifelse(data == "Strongly Disagree", 1, ifelse(data == "Disagree", 2, ifelse(data == "Neither Agree nor Disagree", 3, ifelse(data == "Agree", 4, ifelse(data == "Strongly Agree", 5, NA)))))

colnames(data) <- c("Beescape is simple", "I would use Beescape frequently", "Beescape is easy to use", "I could use beescape without technical support from another person", "The various functions of Beescape are well integrated", "The Beescape interface is consistent", "Most people could learn to use Beescape quickly", "Beescape provides an intuitive interface", "I feel confident using Beescape")

data <- data[,c(2,5,6,8,1,4,9,7,3)] 

##Convert the data to a choice:response format that includes all responses 
data.melt <- melt(data)
data.melt <- data.melt[,2:3]
colnames(data.melt) <- c("choice", "response")
data.melt$response <- as.factor(data.melt$response)
df = data.melt
df = count(df,c('choice','response'))


# Get percentages 
df = ddply(df, .(choice), transform, percent = freq/sum(freq) * 100)
df$label = paste0(sprintf("%.0f", df$percent), "%")

##colorF = c("1"="white", "2"= "black","3"="black", "4"="black", "5"="white")

a = ggplot(data=subset(df, !is.na(response)), aes(x = factor(choice), y = freq, fill = response)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = .7) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank(),
        text = element_text(family = "trebuchet ms")) +
  labs(x = "", y = "Count", colour = "") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
  theme(legend.title = element_blank(), plot.margin = unit(c(.5, 0, .5, .5), "cm")) +
  scale_fill_manual(values = c(sd,di,n,ag,sa),labels =
                      c("Strongly Disagree", "Disagree", "Neither Agree Nor   Disagree", "Agree", "Strongly Agree"), drop = FALSE) +
  geom_text(aes(label = label, color=response, family = "trebuchet ms"),position = position_stack(vjust = 0.5, reverse = TRUE), size = 2) +
  scale_color_manual(values = c("white","black","black","black","white"))+ 
  scale_x_discrete(position = "top")

#Convert response to numeric to allow for plotting 
data.melt$response <- as.numeric(data.melt$response)

b <- ggplot(data=subset(data.melt, !is.na(response)), aes(x = choice, y = response)) +
  geom_boxplot(alpha=0.7, outlier.shape = NA) +
  stat_summary(geom = "crossbar", fun.y = "mean", color = "gray", width = .75, fatten = 2) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_text(hjust = 0.5, size = 8), text = element_text(family = "trebuchet ms"),
        plot.margin = unit(c(.5, .5, .5, 0), "cm")) +
  labs(x = "", y = "Score Distribution ", colour = "") +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(1,5))


#Create arrangement of figures with the legend on the right 
c <- ggarrange(a,b, ncol = 2, nrow = 1, widths = c(2.56,4), heights = c(2,2), legend = "none", common.legend=TRUE)

ggsave(file="q6.svg", plot=c, width=15, height=3)

###############################################################################
###QUESTION 7###
##############################################################################

data <- data.full[,45:51]

data <- ifelse(data == "Strongly disagree", 1, ifelse(data == "Disagree", 2, ifelse(data == "Neither agree nor disagree", 3, ifelse(data == "Agree", 4, ifelse(data == "Strongly agree", 5, NA)))))

colnames(data) <- c("Beescape will help me make better decisions about beekeeping", "I would recommend using Beescape to other beekeepers", "I trust what I see when using Beescape", "It's worth taking time away from other responsibilities to use Beescape", "I would like to integrate Beescape into my beekeeping workflow over time", "The information Beescape provides is relevant to my beekeeping practices", "Beescape will help me connect to what is happening in beekeeping science")

data <- data[,c(1,4,5,3,7,6,2)]

##Convert the data to a choice:response format that includes all responses 
data.melt <- melt(data)
data.melt <- data.melt[,2:3]
colnames(data.melt) <- c("choice", "response")
data.melt$response <- as.factor(data.melt$response)
df = data.melt
df = count(df,c('choice','response'))


# Get percentages 
df = ddply(df, .(choice), transform, percent = freq/sum(freq) * 100)
df$label = paste0(sprintf("%.0f", df$percent), "%")

##colorF = c("1"="white", "2"= "black","3"="black", "4"="black", "5"="white")

a = ggplot(data=subset(df, !is.na(response)), aes(x = factor(choice), y = freq, fill = response)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = .7) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank(),
        text = element_text(family = "trebuchet ms")) +
  labs(x = "", y = "Count", colour = "") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
  theme(legend.title = element_blank(), plot.margin = unit(c(.5, 0, .5, .5), "cm")) +
  scale_fill_manual(values = c(sd,di,n,ag,sa),labels =
                      c("Strongly Disagree", "Disagree", "Neither Agree Nor   Disagree", "Agree", "Strongly Agree"), drop = FALSE) +
  geom_text(aes(label = label, color=response, family = "trebuchet ms"),position = position_stack(vjust = 0.5, reverse = TRUE), size = 2) +
  scale_color_manual(values = c("white","black","black","black","white"))+ 
  scale_x_discrete(position = "top")

#Convert response to numeric to allow for plotting 
data.melt$response <- as.numeric(data.melt$response)

b <- ggplot(data=subset(data.melt, !is.na(response)), aes(x = choice, y = response)) +
  geom_boxplot(alpha=0.7, outlier.shape = NA) +
  stat_summary(geom = "crossbar", fun.y = "mean", color = "gray", width = .75, fatten = 2) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_text(hjust = 0.5, size = 8), text = element_text(family = "trebuchet ms"),
        plot.margin = unit(c(.5, .5, .5, 0), "cm")) +
  labs(x = "", y = "Score Distribution ", colour = "") +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits = c(1,5))


#Create arrangement of figures with the legend on the right 
c <- ggarrange(a,b, ncol = 2, nrow = 1, widths = c(2.56,4), heights = c(2,2), legend = "none", common.legend=TRUE)

ggsave(file="q7.svg", plot=c, width=15, height=3)

###############################################################################
###QUESTION 8###
##############################################################################

data <- data.full[,52:58]

data <- ifelse(data == 1, 1, ifelse(data == 2, 2, ifelse(data == 3, 3, ifelse(data == 4, 4, ifelse(data == 5, 5, ifelse(data == 6, 6, ifelse(data == 7, 7, NA)))))))

colnames(data) <- c("Allow users to view the foraging range (3-5 km) around each apiary on the map", "Allow users to visualize nearby apiaries on the map", "Allow users to visualize how scores will change in the future for their apiary", "Add discussion boards to support beekeeper conversation about landscape health and sustainability", "Allow data from Beescape to be exported for use in other tools", "View management tips for new beekeepers", "View how data collection is helping scientists at Penn State")

data <- data[,c(5,7,4,3,6,2,1)]

##Convert the data to a choice:response format that includes all responses 
data.melt <- melt(data)
data.melt <- data.melt[,2:3]
colnames(data.melt) <- c("choice", "response")
data.melt$response <- as.factor(data.melt$response)
df = data.melt
df = count(df,c('choice','response'))


# Get percentages 
df = ddply(df, .(choice), transform, percent = freq/sum(freq) * 100)
df$label = paste0(sprintf("%.0f", df$percent), "%")

##colorF = c("1"="white", "2"= "black","3"="black", "4"="black", "5"="white")

a = ggplot(data=subset(df, !is.na(response)), aes(x = factor(choice), y = freq, fill = response)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = .7) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_blank(),
        text = element_text(family = "trebuchet ms")) +
  labs(x = "", y = "Count", colour = "") +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30), limits = c(0,30)) +
  theme(legend.title = element_blank(), plot.margin = unit(c(.5, 0, .5, .5), "cm")) +
  scale_fill_brewer(palette="Blues", direction = -1,labels = c("First Choice", "Second Choice", "Third Choice", "Fourth Choice", "Fifth Choice", "Sixth Choice", "Seventh Choice")) +
  geom_text(aes(label = label, color=response, family = "trebuchet ms"),position = position_stack(vjust = 0.5, reverse = TRUE), size = 2) +
  scale_color_manual(values = c("white","white","black","black","black","black","black"))+ 
  scale_x_discrete(position = "top")

#Convert response to numeric to allow for plotting 
data.melt$response <- as.numeric(data.melt$response)

b <- ggplot(data=subset(data.melt, !is.na(response)), aes(x = choice, y = response)) +
  geom_boxplot(alpha=0.7, outlier.shape = NA) +
  stat_summary(geom = "crossbar", fun.y = "mean", color = "gray", width = .75, fatten = 2) +
  coord_flip() +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y=element_text(hjust = 0.5, size = 8), text = element_text(family = "trebuchet ms"),
        plot.margin = unit(c(.5, .5, .5, 0), "cm")) +
  labs(x = "", y = "Score Distribution ", colour = "") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7), limits = c(1,7))


#Create arrangement of figures with the legend on the right 
c <- ggarrange(a,b, ncol = 2, nrow = 1, widths = c(2.56,4), heights = c(2,2), legend = "none", common.legend=TRUE)

ggsave(file="q8.svg", plot=c, width=15, height=3)



