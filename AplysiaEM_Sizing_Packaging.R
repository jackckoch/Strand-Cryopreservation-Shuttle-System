#tell R where your data lives
setwd("/Users/jackckoch/Documents/00_AGGRC/2_Manuscripts/AplysiaEM_Sizing_Packaging")

#install packages
install.packages(c("gee","rstatix", "patchwork", "hrbrthemes","tidymodels"))

#load necessary packages for graphing and statistics
library("rstatix")
library("reshape")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("plyr")
library("datarium")
library("gee")
library("car")
library("multcomp")
library("grid")
library("ggplot2")
library("plotly")
library("reshape2")
library("gplots")
library("FSA")
library("lattice")
library("rcompanion")
library("multcompView")
library("patchwork")
library("hrbrthemes")
library("tidymodels")

#functions
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#load data and check it out
untangle<-read.csv("untangling_data.csv", header=T) #loads untangle data from .csv
print(untangle) #prints all data in untangle
cut<-read.csv("cutting_data.csv", header=T)
print(cut)
process<-read.csv("processing_data.csv", header=T)
print(process)

#####Untangle#####
head(untangle) #check out the first six rows of data
untangle %>%
  summarise(strand.count = sum(count), time.sec.sum = sum(time.sec), time.min.sum = time.sec.sum/60, strand.per.min.avg = strand.count/time.min.sum)
#untangled enough strand pieces in 207 min to cut 2042-1 cm strands or about 10-1 cm strands per min of untangling

untangle %>%
  group_by(percieved.difficulty) %>% #group by percieved difficulty of strand untangling
  summarise(strand.count = sum(count), time.sec.sum = sum(time.sec), time.min.sum = time.sec.sum/60, strand.per.min.avg = strand.count/time.min.sum)
#easy difficulty egg masses: untangled enough strand pieces in 35 min to cut 530-1 cm strands or about 15.2 strands per min of untangling
#medium difficulty egg masses: untangled enough strand pieces in 112 min to cut 1184-1 cm strands or about 10.6 strands per min of untangling
#hard difficulty egg masses: untangled enough strand pieces in 60 min to cut 328-1 cm strands or about 5.5 strands per min of untangling


#####Cutting#####
head(cut)
cutSummary <- cut %>%
  group_by(device) %>%
  summarise(sizedavg = mean(count), time.sec.sum = sum(time.sec), time.min.sum = time.sec.sum/60, strand.count = sum(count), strand.per.min.avg = strand.count/time.min.sum)
cutSummary
#Using a tape ruler to measure 1 cm strands, 972 strands were produced in 182 minutes or an average of 5.3 strands per minute
#Using the Centi-Sizer (Aplysia Bar) to measure and cut 1 cm strands, 979 strands were produced in 161 minutes or an average of 6 strands per minute
cutPercentChange <- cutSummary %>%
  mutate(pct_change1 = (strand.per.min.avg/lag(strand.per.min.avg)-1)* 100)
cutPercentChange
#An 12% increase in strands per minute using the CentiSizer, or 60 additional strands per hour

#scatter plot with linear models
cut$device <- as.factor(cut$device)

lm <-  cut %>% group_by(device) %>% do(model = lm(time.sec~count, data = .))
summary(lm[[2]][[1]]) #Aplysia bar
summary(lm[[2]][[2]]) #Ruler

cuttingplot <- ggplot(data=cut, aes(x=count, y=time.sec)) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 12), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.text.x = element_text(size = rel(1.2)), 
        axis.text.y = element_text(size = rel(1.2)), legend.position = "top") +
  labs(x="1-cm Strand Count",y="Time (s)") +
  geom_smooth(method = "lm", aes(fill=device, color=device)) + #adds linear model with standard error predictions to each symbiotic state
  scale_fill_manual(values=c("azure3","azure3")) +
  geom_jitter(aes(shape=device,color=device), width = 0.2, height = 0, size = 2) +
  scale_color_manual(values=c("#5684E9","#CC79A7")) +
  scale_y_continuous(breaks=seq(0,350,50)) +
  scale_x_continuous(breaks=seq(0,32,2))
cuttingplot

#histogram of sizes
cuttinghisto <- ggplot(data=cut, aes(x=count, fill=device)) +
  geom_histogram(color="black") +
  scale_fill_manual(values=c("#5684E9","#CC79A7")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 12), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.text.x = element_text(size = rel(1.2)), 
        axis.text.y = element_text(size = rel(1.2)), legend.position = "none") +
  labs(x="1-cm Strand Count",y="Count") +
  scale_y_continuous(breaks=seq(0,120,20)) +
  scale_x_continuous(breaks=seq(0,30,5))
cuttinghisto

#inset histogram on scatterplot
cuttingplot + annotation_custom(ggplotGrob(cuttinghisto), xmin = -0.5, xmax = 18, ymin = 210, ymax = 390)

#####Processing#####
head(process)
processSummaryPerStep <- process %>%
  group_by(group, operation, step) %>%
  summarise(steptime.sec.avg = mean(time.sec), steptime.sec.sd = sd(time.sec), strand.per.straw = mean(strand.num), steptime.perstrand.sec.avg = steptime.sec.avg/strand.per.straw) #per step
processSummaryPerStep

processSummaryPerStepNorm <- process %>%
  mutate(steptime.perstrand.sec.norm = time.sec/strand.num) %>%
  group_by(group, operation, step) %>%
  summarise(steptime.sec.avg.norm = mean(steptime.perstrand.sec.norm), steptime.sec.sd.norm = sd(steptime.perstrand.sec.norm)) #per step normalized to number of strands in each straw
processSummaryPerStepNorm

processSummaryPerOperation <- process %>%
  group_by(avg.group, group, step) %>%
  summarise(time.sec.avg = mean(time.sec), time.sec.sd = sd(time.sec)) #per operation of steps
processSummaryPerOperation

processSummaryPerMethod <- process %>%
  group_by(group) %>%
  summarise(method.time.sec.avg = mean(time.sec), strand.per.straw = mean(strand.num), method.time.sec.avg.strand = method.time.sec.avg/strand.per.straw)
processSummaryPerMethod

processPercentChange <- processSummaryPerMethod %>%
  mutate(pct_change_straw = (method.time.sec.avg/lead(method.time.sec.avg)-1)* 100, pct_change_strand = (method.time.sec.avg.strand/lead(method.time.sec.avg.strand)-1)* 100)
processPercentChange

#per step graph
processstepbar <- ggplot(data=process, aes(x=factor(step, level=c('Straw filling', 'Loading straws', 'Sealing Straws', 'Unloading straws')), 
                                           y=time.sec, fill=operation)) +
  geom_dotplot(binaxis='y', stackdir = 'center', position = position_dodge(0.8), binwidth = 1, dotsize = 5) +
  scale_fill_manual(values=c("#5684E9", "#5684E9", "#5684E9", "#CC79A7", "#CC79A7", "#CC79A7", "#5684E9","#CC79A7")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 12), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.text.x = element_text(size = rel(1.2)), 
        axis.text.y = element_text(size = rel(1.2)), legend.position = "top") +
  labs(x="Strand Processing Step",y="Time (s)") +
  #stat_summary(fun.data=data_summary, color="red", position = position_dodge(0.8)) +
  scale_y_continuous(breaks=seq(0,350,50))
processstepbar

#per step graph normalized to number of strands per straw
processSummaryPerStepNorm <- process %>%
  mutate(steptime.perstrand.sec.norm = time.sec/strand.num)
processSummaryPerStepNorm

processstepnormbar <- ggplot(data=processSummaryPerStepNorm, aes(x=factor(step, level=c('Straw filling', 'Loading straws', 'Sealing Straws', 'Unloading straws')), 
                           y=steptime.perstrand.sec.norm, fill=operation)) +
  geom_dotplot(binaxis='y', stackdir = 'center', position = position_dodge(0.8), binwidth = 1, dotsize = 0.4) +
  scale_fill_manual(values=c("#5684E9", "#5684E9", "#5684E9", "#CC79A7", "#CC79A7", "#CC79A7", "#5684E9","#CC79A7")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 12), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.text.x = element_text(size = rel(1.2)), 
        axis.text.y = element_text(size = rel(1.2)), legend.position = "none") +
  labs(x="Strand Processing Step",y="Time Per Strand (s)") +
  stat_summary(fun.data=data_summary, color="black",shape=18, position = position_dodge(0.8)) +
  scale_y_continuous(breaks=seq(0,15,3))
processstepnormbar
         