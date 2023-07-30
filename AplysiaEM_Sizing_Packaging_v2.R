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

#untangling statistics
#Inspect the data
histogram(~  time.sec | percieved.difficulty,data=untangle,layout=c(1,3)) #low sample size, normality difficult to assess visually

#Statistical assumptions
#Q-Q Plot (Normality)
ggqqplot(untangle$time.sec) #normality looks ok

#Shapiro-Wilks test for normality
shapiro.test(untangle$time.sec) #p = 0.0008, total data not normal
untangle$time.sec.log <- log(untangle$time.sec)
untangle$time.sec.sqrt <- sqrt((untangle$time.sec)+1)
shapiro.test(untangle$time.sec.log) #p = 0.0007, total data not normal
shapiro.test(untangle$time.sec.sqrt) #p = 0.0008, total data not normal
#use non-parametric anova - Kruskal-Wallis one-way ANOVA

#Levene's test for homogeneity of variance
leveneTest(time.sec~as.factor(percieved.difficulty), data = untangle)
#Reject null, p = 0.0018, variances unequal, use non-parametric anova - Kruskal-Wallis one-way ANOVA

#Kruskal-Wallis one-way ANOVA
kruskal.test(time.sec~percieved.difficulty, data=untangle)

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
  labs(x="1-cm Strand Count",y="Number of Occurrences") +
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

#Processing Statistics processSummaryPerStepNorm
#prepare column for statistics
processSummaryPerStepNorm <- processSummaryPerStepNorm %>%
  unite("stepgroup", step:group, sep="-", remove = FALSE)
#Inspect the data
histogram(~  steptime.perstrand.sec.norm | stepgroup,data=processSummaryPerStepNorm,layout=c(2,4)) #Some groups look normal, other groups do not

#Statistical assumptions
#Q-Q Plot (Normality)
ggqqplot(processSummaryPerStepNorm$steptime.perstrand.sec.norm) #normality looks ok

#Shapiro-Wilks test for normality
shapiro.test(processSummaryPerStepNorm$steptime.perstrand.sec.norm) #p = 0.0008, data not normal
#per conc
shapiro <- processSummaryPerStepNorm %>%
  group_by(stepgroup) %>%
  do(tidy(shapiro.test(.$steptime.perstrand.sec.norm)))
shapiro #some group steps are normal, some are not

#Levene's test for homogeneity of variance
leveneTest(steptime.perstrand.sec.norm~as.factor(stepgroup), data = processSummaryPerStepNorm)
#reject null, p = 0.002, variances unequal, use non-parametric ANOVA

#Kruskal-Wallis one-way ANOVA
kruskal.test(steptime.perstrand.sec.norm~stepgroup, data=processSummaryPerStepNorm)
#p > 0.0001, there are significant differences

#Dunn test posthoc
process_dunnposthoc <- processSummaryPerStepNorm %>%
  group_by(stepgroup) %>%
  dunnTest(
    steptime.perstrand.sec.norm ~ stepgroup,
    data = .,
    method = "holm"
  )
process_dunnposthoc
#Dunn (1964) Kruskal-Wallis multiple comparison p-values adjusted with the Holm method.
#                                            Comparison          Z      P.unadj        P.adj
#15       Straw filling-Cassette - Straw filling-Plunge -4.2718689 1.938415e-05 4.846038e-04
#1      Loading straws-Cassette - Loading straws-Plunge -2.3255295 2.004366e-02 2.806113e-01
#6      Sealing Straws-Cassette - Sealing Straws-Plunge -2.5149546 1.190477e-02 2.023811e-01
#28 Unloading straws-Cassette - Unloading straws-Plunge -2.9137424 3.571245e-03 7.142489e-02

#straw filling adj p > 0.001
#loading straw adj p = 0.28
#sealing straws adj p = 0.2
#unloading straws adj p = 0.07

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

#per step graph normalized to number of strands per straw MEANS
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

#per step graph normalized to number of strands per straw MEDIANS
processstepnormbar2 <- ggplot(data=processSummaryPerStepNorm, aes(x=factor(step, level=c('Straw filling', 'Loading straws', 'Sealing Straws', 'Unloading straws')), 
                                                                 y=steptime.perstrand.sec.norm, fill=operation)) +
  geom_dotplot(binaxis='y', stackdir = 'center', position = position_dodge(0.8), binwidth = 1, dotsize = 0.4) +
  geom_boxplot() +
  scale_fill_manual(values=c("#5684E9", "#5684E9", "#5684E9", "#CC79A7", "#CC79A7", "#CC79A7", "#5684E9","#CC79A7")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 12), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.text.x = element_text(size = rel(1.2)), 
        axis.text.y = element_text(size = rel(1.2)), legend.position = "none") +
  labs(x="Strand Processing Step",y="Time Per Strand (s)") +
  scale_y_continuous(breaks=seq(0,15,3))
processstepnormbar2

#calculate medians and interquantile ranges
processSummaryPerStepNorm <- processSummaryPerStepNorm %>%
  unite("stepgroup", step:group, sep="-", remove = FALSE)

processSummaryPerStepNormMEDIANQUARTILES <- processSummaryPerStepNorm %>%
  group_by(stepgroup) %>%
  summarise(med = median(steptime.perstrand.sec.norm),
            Q1 = quantile(steptime.perstrand.sec.norm, 0.25),
            Q3 = quantile(steptime.perstrand.sec.norm, 0.75))
processSummaryPerStepNormMEDIANQUARTILES         
