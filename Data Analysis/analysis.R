rm(list = ls())

library(plyr)
library(dplyr)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(rstatix)
library(emmeans)
library(phia)       #testInteractions
library(tidyr)      #spread
library(ARTool)     #art, artlm
library(coin)
library(tidyverse) #add column

#average errors: recalibrated with calibration task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_calibration_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)
average_error_data$id = factor(average_error_data$id)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

long_a_average_error$ErrorType = factor(long_a_average_error$ErrorType)
long_a_average_error$taskname = factor(long_a_average_error$taskname)
long_b_average_error$ErrorType = factor(long_b_average_error$ErrorType)
long_b_average_error$taskname = factor(long_b_average_error$taskname)

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))

#plot dataframes for cosine error
ggplot(a_average_error, aes(x=taskname, y=cosineError)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean", fill='goldenrod') + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Cosine Error (degrees)')
ggplot(b_average_error, aes(x=taskname, y=cosineError)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean", fill='goldenrod') + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Cosine Error (degrees)')

#One way repeated measures (within subjects) ANOVA: find differences in accuracy between tasks
result <- aov(cosineError ~ taskname + Error(id), data = a_average_error)
summary(result)
attach(a_average_error)
posthoc <- pairwise.t.test(cosineError,taskname, p.adj = "bonf", paired = TRUE)
posthoc
detach(a_average_error)
posthoc <- a_average_error %>% pairwise_t_test(cosineError ~ taskname, paired = TRUE, p.adjust.method = "bonferroni")
posthoc

result <- aov(cosineError ~ taskname + Error(id), data = b_average_error)
summary(result)
attach(b_average_error)
posthoc <- pairwise.t.test(cosineError,taskname, p.adj = "bonf", paired = TRUE)
posthoc
detach(b_average_error)

#Two way mixed ANOVA: determine if headset makes a difference
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/
result <- anova_test(data = average_error_data, dv = cosineError, wid=id, between=hololens, within=taskname)
get_anova_table(result)

model <- art(data = average_error_data, cosineError ~ taskname * hololens + Error(id))
anova(model)


#Two way ANOVA for calibration
result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_a_average_error)
summary(result)

model <- art(data = long_a_average_error, ErrorValue ~ taskname * ErrorType + Error(id))
anova(model)

posthoc <- long_a_average_error %>% group_by(taskname) %>% pairwise_t_test(ErrorValue ~ ErrorType, paired = TRUE, p.adjust.method = "bonferroni")
posthoc

test = data.frame(long_a_average_error)
posthoc <- long_a_average_error %>% group_by(taskname) %>% wilcox_test(ErrorValue ~ ErrorType, paired=FALSE, exact=FALSE, p.adjust.method="Bonferroni")
posthoc





result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_b_average_error)
summary(result)


#average errors: recalibrated with ssHeadConstrained task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_ssHeadConstrained_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)
average_error_data$id = factor(average_error_data$id)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

long_a_average_error$ErrorType = factor(long_a_average_error$ErrorType)
long_a_average_error$taskname = factor(long_a_average_error$taskname)
long_b_average_error$ErrorType = factor(long_b_average_error$ErrorType)
long_b_average_error$taskname = factor(long_b_average_error$taskname)

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))

#Two way ANOVA
result <- aov(ErrorValue ~ taskname * ErrorType  + Error(id/(taskname * ErrorType)), data = long_a_average_error)
summary(result)
posthoc <- long_a_average_error %>% group_by(taskname) %>% pairwise_t_test(ErrorValue ~ ErrorType, paired = TRUE, p.adjust.method = "bonferroni")
posthoc

result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_b_average_error)
summary(result)



#average errors: recalibrated with wsBodyConstrained task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_wsBodyConstrained_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)
average_error_data$id = factor(average_error_data$id)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))

#Two way ANOVA
result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_a_average_error)
summary(result)
posthoc <- long_a_average_error %>% group_by(taskname) %>% pairwise_t_test(ErrorValue ~ ErrorType, paired = TRUE, p.adjust.method = "bonferroni")
posthoc

result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_b_average_error)
summary(result)





#average errors: recalibrated with ssWalking task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_ssWalking_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)
average_error_data$id = factor(average_error_data$id)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))

#Two way ANOVA
result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_a_average_error)
summary(result)
posthoc <- long_a_average_error %>% group_by(taskname) %>% pairwise_t_test(ErrorValue ~ ErrorType, paired = TRUE, p.adjust.method = "bonferroni")
posthoc

result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_b_average_error)
summary(result)





#average errors: recalibrated with wsWalking task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_wsWalking_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)
average_error_data$id = factor(average_error_data$id)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))

#Two way ANOVA
result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_a_average_error)
summary(result)
posthoc <- long_a_average_error %>% group_by(taskname) %>% pairwise_t_test(ErrorValue ~ ErrorType, paired = TRUE, p.adjust.method = "bonferroni")
posthoc

result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_b_average_error)
summary(result)



#average errors: recalibrated with hallway task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_hallway_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)
average_error_data$id = factor(average_error_data$id)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))

#Two way ANOVA
result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_a_average_error)
summary(result)
posthoc <- long_a_average_error %>% group_by(taskname) %>% pairwise_t_test(ErrorValue ~ ErrorType, paired = TRUE, p.adjust.method = "bonferroni")
posthoc

result <- aov(ErrorValue ~ taskname * ErrorType + Error(id/(taskname * ErrorType)), data = long_b_average_error)
summary(result)



#average errors: static recalibrated with calibration task
average_moving_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/static_recalibrated_calibration_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')
#average errors: moving recalibrated with calibration task
average_static_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/moving_recalibrated_calibration_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#add column to specify movement
average_moving_error_data <- average_moving_error_data %>% add_column(movement = 'moving')
average_static_error_data <- average_static_error_data %>% add_column(movement = 'static')

#turn variables into factors
average_moving_error_data$taskname = factor(average_moving_error_data$taskname)
average_moving_error_data$hololens = factor(average_moving_error_data$hololens)
average_moving_error_data$movement = factor(average_moving_error_data$movement)
average_moving_error_data$id = factor(average_moving_error_data$id)
average_static_error_data$taskname = factor(average_static_error_data$taskname)
average_static_error_data$hololens = factor(average_static_error_data$hololens)
average_static_error_data$movement = factor(average_static_error_data$movement)
average_static_error_data$id = factor(average_static_error_data$id)

#merge dataframes
total_error = rbind(average_moving_error_data, average_static_error_data)

#separate between HoloLenses
a_average_error <- total_error[total_error$hololens=='A',]
b_average_error <- total_error[total_error$hololens=='B',]

#plot cosine error
ggplot(a_average_error, aes(x=taskname, y=cosineError, fill=movement)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Cosine Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))
ggplot(b_average_error, aes(x=taskname, y=cosineError, fill=movement)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Cosine Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))

#Two way ANOVA on cosine error
result <- aov(cosineError ~ taskname * movement + Error(id/(taskname * movement)), data = a_average_error)
summary(result)
posthoc <- a_average_error %>% group_by(taskname) %>% pairwise_t_test(cosineError ~ movement, paired = TRUE, p.adjust.method = "bonferroni")
posthoc

result <- aov(cosineError ~ taskname * movement + Error(id/(taskname * movement)), data = b_average_error)
summary(result)


#plot uncalibrated euclidean error
ggplot(a_average_error, aes(x=taskname, y=euclideanError, fill=movement)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Uncalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))
ggplot(b_average_error, aes(x=taskname, y=euclideanError, fill=movement)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Uncalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))

#Two way ANOVA on uncalibrated euclidean error
result <- aov(euclideanError ~ taskname * movement + E, data = a_average_error)
summary(result)
posthoc <- TukeyHSD(result, which="taskname:movement")
posthoc

result <- aov(euclideanError ~ taskname * movement, data = b_average_error)
summary(result)
posthoc <- TukeyHSD(result, which="taskname:movement")
posthoc


#plot recalibrated euclidean error
ggplot(a_average_error, aes(x=taskname, y=recalibratedEuclideanError, fill=movement)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Recalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))
ggplot(b_average_error, aes(x=taskname, y=recalibratedEuclideanError, fill=movement)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Recalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))

#Two way ANOVA on recalibrated euclidean error
result <- aov(recalibratedEuclideanError ~ taskname * movement, data = a_average_error)
summary(result)
posthoc <- TukeyHSD(result, which="taskname:movement")
posthoc

result <- aov(recalibratedEuclideanError ~ taskname * movement, data = b_average_error)
summary(result)
posthoc <- TukeyHSD(result, which="taskname:movement")
posthoc


#average errors: static recalibrated with wsBodyConstrained task
average_moving_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/static_recalibrated_wsBodyConstrained_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')
#average errors: moving recalibrated with calibration task
average_static_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/moving_recalibrated_wsBodyConstrained_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#add column to specify movement
average_moving_error_data <- average_moving_error_data %>% add_column(movement = 'moving')
average_static_error_data <- average_static_error_data %>% add_column(movement = 'static')

#turn variables into factors
average_moving_error_data$taskname = factor(average_moving_error_data$taskname)
average_moving_error_data$hololens = factor(average_moving_error_data$hololens)
average_moving_error_data$movement = factor(average_moving_error_data$movement)
average_static_error_data$taskname = factor(average_static_error_data$taskname)
average_static_error_data$hololens = factor(average_static_error_data$hololens)
average_static_error_data$movement = factor(average_static_error_data$movement)

#merge dataframes
total_error = rbind(average_moving_error_data, average_static_error_data)

#separate between HoloLenses
a_average_error <- total_error[total_error$hololens=='A',]
b_average_error <- total_error[total_error$hololens=='B',]

#plot recalibrated euclidean error
ggplot(a_average_error, aes(x=taskname, y=recalibratedEuclideanError, fill=movement)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Recalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))
ggplot(b_average_error, aes(x=taskname, y=recalibratedEuclideanError, fill=movement)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Recalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))




#average errors: session 1 recalibrated with calibration task
average_session1_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/session1_recalibrated_calibration_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')
#average errors: session 2 recalibrated with calibration task
average_session2_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/session2_recalibrated_calibration_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#add column to specify movement
average_session1_error_data <- average_session1_error_data %>% add_column(trial = 1)
average_session2_error_data <- average_session2_error_data %>% add_column(trial = 2)

#turn variables into factors
average_session1_error_data$taskname = factor(average_session1_error_data$taskname)
average_session1_error_data$hololens = factor(average_session1_error_data$hololens)
average_session1_error_data$trial = factor(average_session1_error_data$trial)
average_session1_error_data$id = factor(average_session1_error_data$id)
average_session2_error_data$taskname = factor(average_session2_error_data$taskname)
average_session2_error_data$hololens = factor(average_session2_error_data$hololens)
average_session2_error_data$trial = factor(average_session2_error_data$trial)
average_session2_error_data$id = factor(average_session2_error_data$id)

#merge dataframes
total_error = rbind(average_session1_error_data, average_session2_error_data)

#separate between HoloLenses
a_average_error <- total_error[total_error$hololens=='A',]
b_average_error <- total_error[total_error$hololens=='B',]

#plot cosine error
ggplot(a_average_error, aes(x=taskname, y=cosineError, fill=trial)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Cosine Error (degrees)') + scale_fill_discrete(name="", breaks=c('1', '2'), labels=c('Trial 1', 'Trial 2'))
ggplot(b_average_error, aes(x=taskname, y=cosineError, fill=trial)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Cosine Error (degrees)') + scale_fill_discrete(name="", breaks=c('1', '2'), labels=c('Trial 1', 'Trial 2'))

#Two way ANOVA on cosine error
result <- aov(cosineError ~ taskname * trial + Error(id/(taskname * trial)), data = a_average_error)
summary(result)
attach(a_average_error)
posthoc <- pairwise.t.test(cosineError, taskname, p.adj = "bonf", paired = TRUE)
posthoc
detach(a_average_error)

result <- aov(cosineError ~ taskname * trial + Error(id/(taskname * trial)), data = b_average_error)
summary(result)


#plot uncalibrated euclidean error
ggplot(a_average_error, aes(x=taskname, y=euclideanError, fill=trial)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Uncalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('1', '2'), labels=c('Trial 1', 'Trial 2'))
ggplot(b_average_error, aes(x=taskname, y=euclideanError, fill=trial)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Uncalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('1', '2'), labels=c('Trial 1', 'Trial 2'))

#Two way ANOVA on uncalibrated euclidean error
result <- aov(euclideanError ~ taskname * trial + Error(id/(taskname * trial)), data = a_average_error)
summary(result)

result <- aov(euclideanError ~ taskname * trial + Error(id/(taskname * trial)), data = b_average_error)
summary(result)

#plot recalibrated euclidean error
ggplot(a_average_error, aes(x=taskname, y=recalibratedEuclideanError, fill=trial)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Recalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))
ggplot(b_average_error, aes(x=taskname, y=recalibratedEuclideanError, fill=trial)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Recalibrated Euclidean Error (degrees)') + scale_fill_discrete(name="", breaks=c('moving', 'static'), labels=c('Moving', 'Static'))


