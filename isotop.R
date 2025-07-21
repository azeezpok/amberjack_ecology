##########isotope analysis for Amberjack fish#######
setwd("D:/isotop")

#___________________________________________________________________________________________
########install packages###############
#___________________________________________________________________________________________
library(rjags)
library(tRophicPosition)
library(ggplot2)
library(reshape2)
library(plyr)
library(RColorBrewer)
library(stats)
library(multcomp)
library(ggsignif)
library(multcompView)
library(broom)
library(datasets)

#___________________________________________________________________________________________
######## Data ###############
#___________________________________________________________________________________________
data.sum<-read.csv("sum_isotope.csv")
head(data.sum)

#prepare X & Y axis
Ylims <- aes(ymax = d15Nmn + d15Nsd, ymin=d15Nmn - d15Nsd)
Xlims <- aes(xmax = d13Cmn + d13Csd, xmin=d13Cmn - d13Csd)

# Define custom shapes
my_shapes <- c(0,1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)

#___________________________________________________________________________________________
######## Basic plots ###############
#___________________________________________________________________________________________

sum.biplot<-ggplot(data.sum, aes(x=d13Cmn, y=d15Nmn, colour=Class,shape=Class)) + 
  geom_point(size=3) + 
  scale_shape_manual(values = my_shapes)+
  geom_errorbar(Ylims, width=0.2) + 
  geom_errorbarh(Xlims, height=0.2) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  scale_x_continuous(breaks = seq(-22.5, -12.5, 1)) +
  scale_y_continuous(breaks = seq(5, 15, 1)) +
  #facet_grid(. ~ River)+
  theme_bw()
sum.biplot

#jpeg("isotope_plot.jpg", res = 600,height = 7,width = 9,units = "in")
sum.biplot
dev.off()

######produce N and C value for Chlorostoma argyrostoma as baseline########
tail(data.sum)
ca_Nmn<- 6.24
ca_Nsd<- 0.29
N_values <- rnorm(n = 5, mean = ca_Nmn, sd = ca_Nsd)

ca_Cmn<- -14
ca_Csd<- 0.29
C_values <- rnorm(n = 5, mean = ca_Cmn, sd = ca_Csd)

ca_values<-cbind(N_values,C_values)
#write.csv(ca_values,"baseline_values.csv")


########SD plot#############
head(TP.data)
#tp.data_lengthmn<-read.csv("AJ_length_mn.csv")
#season_order <- c("SP1", "SU2", "AU3", "WN4", "CA")
#sex_order<-c("M1","F2","NI3","CA")
length_order<-c("Class I","Class II","Class III","CA")

Ylims <- aes(ymax = d15Nmn + d15Nsd, ymin=d15Nmn - d15Nsd)
Xlims <- aes(xmax = d13Cmn + d13Csd, xmin=d13Cmn - d13Csd)
# Scatter plot colored by groups ("Species")
sp <- ggplot(tp.data_lengthmn, aes(x = d13Cmn, y = d15Nmn, color = Group)) +
  geom_point(size=3) + 
  #scale_shape_manual(values = my_shapes)+
  geom_errorbar(Ylims, width=0.2) + 
  geom_errorbarh(Xlims, height=0.2) +
  ylab(expression(delta^{15}~N)) +
  xlab(expression(delta^{13}~C)) +
  scale_color_discrete(limits = length_order)+ 
  #scale_x_continuous(breaks = seq(-22.5, -12.5, 1)) +
  scale_y_continuous(breaks = seq(5, 15, 1)) +
  #facet_grid(. ~ River)+
  theme_bw()

#jpeg("iso_length.jpg", res = 600,height = 6,width = 7,units = "in")
sp
dev.off()

#___________________________________________________________________________________________
######## modelling TP ###############
#___________________________________________________________________________________________

TP.data<-read.csv("AJ_Length.csv")
head(TP.data)

#prepare isotope data for each group
TDF_values <- TDF()

Lengthist <- extractIsotopeData(TP.data,
                               consumersColumn = "Class",
                               b1 = "CA",
                               b2 = NULL,
                               baselineColumn = "Species",
                               #groupsColumn = "Group",
                               deltaC = TDF_values$deltaC,
                               deltaN = TDF_values$deltaN)

##We can then generate some summary tables and these cool density-biplots 
#which Claudio developed

for (Class in Lengthist){
  plot(Class)
  print(summary(Class))
}

##save plot for each class length#######
# set up the device for high-resolution output
#jpeg("season1.jpg", width = 11, height = 11, units = "in", res = 600)
par(mfrow = c(1, 2))
# plot the first class and print the summary
plot(season1List[[1]])
plot(season1List[[2]])
dev.off()
plot(season1List[[3]])
plot(season1List[[4]])
plot(season1List[[5]])
dev.off()

# repeat for the second class
#png("plot2.jpg", width = 8, height = 6, units = "in", res = 600)
plot(LengthList[[2]])
print(summary(LengthList[[2]]))
dev.off()

# repeat for the third class
#png("plot3.jpg", width = 8, height = 6, units = "in", res = 600)
plot(sexList[[3]])
print(summary(sexList[[3]]))
dev.off()

                                                                                                                                                                                                                                                                                                                          
###estimate TP
# First we create a cluster with the cores available
cluster <- parallel::makePSOCKcluster(parallel::detectCores())
# Then we run the model in parallel, nested in system.time()
# in order to know how much time it takes to finish calculations
system.time(Length_TPmodels <- parallel::parLapply(cluster,
                                                    Lengthist,
                                                    multiModelTP,
                                                    lambda=2, #Trophic position of source
                                                    adapt = 20000,
                                                    n.iter = 20000,
                                                    burnin = 2000,
                                                    n.chains = 5,
                                                    model = "oneBaseline"))

parallel::stopCluster(cluster)

#TP model result summary
ggplot_df <- fromParallelTP(Length_TPmodels, get = "summary")
head(ggplot_df)


####TP summary table arrangement and save#####
out_test<-data.frame()

for (i in unique(row.names(summary(Length_TPmodels)))){
  temp_test<-data.frame()
  temp_test<-data.frame(Model=c(i), TP=Length_TPmodels[[i]]["TP"], 
                        group1=gsub("-.*","",i))
  out_test<-rbind(out_test, temp_test)
}

colnames(out_test)<-c("Model", "TP", "Class")
out_test$Class <- factor(out_test$Class, levels = c("Class I", "ClassII", "ClassIII"))


TP.sum<-ddply(out_test, c("Class"), summarise,
              TP.Mean = round(mean(TP),digit=1),
              TP.SD = round(sd(TP), digit=1))
TP.sum
#write.csv(TP.sum, "TP.sum_season1.csv")
#write.csv(out_test, "out_test_sex.csv")


##########TP plot#########
TP.ffg.plot<-ggplot(out_test, aes(y = TP, x = Class, colour= Class, fill = Class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5)+
  #facet_grid(River ~ .) +
  theme_bw()+
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  ylab("Trophic position") +
  xlab(NULL)+ scale_y_continuous(limits = c(3.4, 4.8))
  #scale_y_continuous(breaks = seq(3.5, 7, 0.3))
#scale_y_continuous(breaks = seq(3.5, 5, 0.3))
#TP.ffg.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")

#jpeg("TP_season1.jpg", res = 600,height = 6,width = 8,units = "in")
TP.ffg.plot
dev.off()

#___________________________________________________________________________________________
######## Anova for comparing mean TP for each group ###############
#___________________________________________________________________________________________

head(out_test)

#out_test$Class <- factor(out_test$Class, 
                         #levels = c("SP1", "SU2", "AU3","WN4","ALL"))

# analysis of variance
anova <- aov(TP ~ Class, data = out_test)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)
tukey_length<-broom::tidy(tukey)
#write.csv(tukey_length,"tukey_length.csv")

# compact letter display
cld <- multcompView::multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(out_test, Class) %>%
  dplyr::summarise(mean = mean(TP),
                   quant = quantile(TP, probs = 0.75))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Class)
Tk$cld <- cld$Letters

print(Tk)

# Display the results using compact letters
group_means <- aggregate(TP ~ Class, data = out_test, mean)

head(out_test)
gg.anova<-ggplot(out_test, aes(y = TP, x = Class, colour= Class, fill = Class)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5)+
  geom_text(data = Tk, aes(x = Class, y = quant, label = cld), 
            size = 5, vjust = -1, hjust = -1) +
  guides(fill = "none") +
  #facet_grid(River ~ .) +
  theme_bw()+
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  ylab("Trophic position") +
  xlab(NULL)+ scale_y_continuous(limits = c(3.4, 4.8))
  
#jpeg("TP_Length_annova.jpg", res = 600,height = 6,width = 8,units = "in")
gg.anova
dev.off()
#___________________________________________________________________________________________
######## THE END ###############
#___________________________________________________________________________________________
