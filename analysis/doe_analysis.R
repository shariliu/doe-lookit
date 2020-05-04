# Shari Liu
# Last Updated Dec 8, 2016
# Analysis Script for: Liu & Spelke. Six-month-old infants expect agents to minimize the cost of their actions. Cognition.

## STEP 0. Load required packages-----------------------------

# create a function to do this more cleanly
ipak <- function (pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = FALSE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("schoRsch", "foreign", "lsr", "irr", "vcd", "ggplot2", "lattice", "reshape2", "car", "plyr", "MASS", "lme4", "lmerTest",
              "effects", "influence.ME", "effsize", "longpower", "multcomp", "lsmeans", "Hmisc", "meta", "tidyr", "dplyr", "psych", "compute.es", "pwr", "lattice", "boot",
              "cowplot")  # package names go here
ipak(packages)

## STEP 1. Import and process data------------------------

# import data
setwd("/Users/shariliu/Documents/HarvardLDS/Studies/DOE/Writing/") # change to your local directory!
doe.wide <- read.csv(file = "doe_data_osf_6dec16.csv", header = TRUE)
str(doe.wide)
names(doe.wide)

# melt data into long format
doe.long <- melt(doe.wide, id.vars = c("sex", "order", "subj", "agem", "experiment", "hab1", "hab2", "hab3", "hab4", "hab5",
                                       "hab6", "hab7", "hab8", "hab9", "hab10", "hab11", "hab12", "test1", "test2", "test3",
                                       "test4", "test5", "test6", "n_hab", "avg_hab", "total_hab", "sumhab123", "sumhab321",
                                        "raw_diff", "pref", "comments"))

# give columns new names
colnames(doe.long)[32:33] <- c("type", "look")

str(doe.long)
doe.long$look <- as.numeric(doe.long$look)

# section data by dependent measure
doe.rawl <- doe.long
doe.rawl$loglook <- log(doe.rawl$look)
doe.rawl <- dplyr::filter(doe.rawl, type == "high_avg" | type == "low_avg")
doe.rawl$type <- factor(doe.rawl$type)
doe.prop <- dplyr::filter(doe.long, type == "high_prop_avg")
doe.bp <- dplyr::filter(doe.long, type == "high1" | type == "high2" |type == "high3" | type == "low1" | type == "low2" | type == "low3")
doe.bp$type <- factor(doe.bp$type)
doe.bp$loglook <- log(doe.bp$look)

# add test pair info -----------------
doe.bp$tp <- rep(NA, nrow(doe.bp))
doe.bp$type.new <- rep(NA, nrow(doe.bp))

for (i in 1:nrow(doe.bp)) {
  if (doe.bp$type[i] == "high1") {
    doe.bp$tp[i] <- "First"
    doe.bp$type.new[i] <- "High"
  }
  else if (doe.bp$type[i] == "low1") {
    doe.bp$tp[i] <- "First"
    doe.bp$type.new[i] <- "Low"
  }
  else if (doe.bp$type[i] == "high2") {
    doe.bp$tp[i] <- "Second"
    doe.bp$type.new[i] <- "High"
  }
  else if (doe.bp$type[i] == "low2") {
    doe.bp$tp[i] <- "Second"
    doe.bp$type.new[i] <- "Low"
  }
  else if (doe.bp$type[i] == "high3") {
    doe.bp$tp[i] <- "Third"
    doe.bp$type.new[i] <- "High"
  }
  else if (doe.bp$type[i] == "low3") {
    doe.bp$tp[i] <- "Third"
    doe.bp$type.new[i] <- "Low"
  }
}

doe.bp$tp <- factor(doe.bp$tp)
doe.bp$type.new <- factor(doe.bp$type.new)

# does normal or lognormal distribution better fit looks?------------
hist(doe.rawl$look, breaks = 10, main = NA, xlab = "Looking Time (sec)")
fitdistr(doe.rawl$look, "normal")$loglik
fitdistr(doe.rawl$look, "log-normal")$loglik

## STEP 2. Visualize data----------------
levels(doe.rawl$type) <- c("High", "Low")


## Retrieved from : http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#error-bars-for-within-subjects-variables
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=TRUE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=TRUE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

## get within-subjects CIs for plotting
summary.avg <- summarySEwithin(data = doe.rawl, measurevar = "look", betweenvars = c("experiment"), withinvars = "type", idvar = "subj")

# Figure 1
doe.rawl$type <- relevel(doe.rawl$type, "High")
plot1 <- ggplot(data = dplyr::filter(doe.rawl), aes(type, look, fill=type))
plot1 + geom_boxplot(outlier.shape=NA) + 
  #geom_jitter(position = position_dodge(.75), size = 1) + 
  geom_point(alpha = 0.3)+
  geom_line(alpha = 0.3, aes(group = subj))+
  xlab("Test Action") +
  ylab("Looking Time (s)") +
  coord_cartesian(ylim = c(0, 45)) +
  stat_summary(fun.y=mean, geom="point", colour="red", size=3) +
  geom_errorbar(data = dplyr::filter(summary.avg), colour="red", position = position_dodge(width = 5), width = 0, aes(ymin=look-ci, ymax=look+ci)) +
  scale_fill_manual(values = c("grey45", "grey90"), name = "Action Height", labels = c("High", "Low")) +
  facet_grid(~experiment) +                                             # use to facet data by another factor
  # theme_bw(25) + 
  theme(legend.position="none")


barplot1 <- ggplot(data = dplyr::filter(doe.rawl, experiment=="Exp.1"), aes(type, look, fill=type))
barplot1 + 
  # geom_boxplot(outlier.shape=NA) + 
  #geom_jitter(position = position_dodge(.75), size = 1) + 
  # geom_point(alpha = 0.3)+
  # geom_line(alpha = 0.3, aes(group = subj))+
  stat_summary(fun.y=mean, geom="bar", colour="black") +
  xlab("Test Action") +
  ylab("Looking Time (s)") +
  coord_cartesian(ylim = c(0, 45)) +
  stat_summary(fun.y=mean, geom="point", colour="red", size=3) +
  geom_errorbar(data = dplyr::filter(summary.avg, experiment=="Exp.1"), colour="red", position = position_dodge(width = 5), width = 0, aes(ymin=look-ci, ymax=look+ci)) +
  scale_fill_manual(values = c("grey45", "grey90"), name = "Action Height", labels = c("High", "Low")) +
  facet_grid(~experiment) +                                             # use to facet data by another factor
  # theme_bw(25) + 
  theme(legend.position="none")

## Supplemental Figures-----------------------------------

# Figure S1. Individual looking times by subject
plot2 <- ggplot(data = doe.rawl, aes(type, look, fill=type, group = subj))
plot2 + geom_line(aes(colour = experiment)) +
  xlab("Test Action") +
  ylab("Looking Time (s)") +
  ylim(0,40) +
  stat_summary(fun.y=median, geom="point", fill="red", shape=20, size=4) +
  facet_grid(~experiment) +                                             # use to facet data by another factor
  theme_bw(25) + 
  theme(text=element_text(family="Helvetica Neue LT Std 57 Condensed"))

# Figure S2. Total looking time during habituation by experiment
plot3 <- ggplot(data = dplyr::filter(doe.rawl), aes(experiment, total_hab, fill=experiment))
plot3 + geom_boxplot(outlier.shape=NA) +#+ geom_jitter(position = position_dodge(.75), size = 1) + 
  xlab("Experiment") +
  ylab("Total Looking durin Habituation (s)") +
  coord_cartesian(ylim = c(0,500)) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=4) +
  theme_bw(25) + 
  theme(text=element_text(family="Helvetica Neue LT Std 57 Condensed"), legend.position="none")

# Figure S3. Looking during test by experiment
plot3 <- ggplot(data = dplyr::filter(doe.rawl), aes(experiment, look, fill=experiment))
plot3+ geom_boxplot(outlier.shape=NA) +
  xlab("Test Action") +
  ylab("Looking Time (s)") +
  coord_cartesian(ylim = c(0, 40)) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=6) +
  theme_bw(25) + 
  theme(text=element_text(family="Helvetica Neue LT Std 57 Condensed"), legend.position="none")

# Figure S4. Looking time per test pair by experiment 
plot3 <- ggplot(data = doe.bp, aes(type.new, look, fill=type.new))
plot3+ geom_boxplot(outlier.shape=NA) +
  xlab("Test Trial") +
  ylab("Looking Time (s)") +
  ylim(0,40) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=6) +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_grid(~experiment+tp) +                                             # use to facet data by another factor
  theme_bw(22) + 
  theme(text=element_text(family="Helvetica Neue LT Std 57 Condensed"), legend.position="none")

# M and SD for raw looking during habituation and test -------------------
aggregate(look ~ experiment + type , data = doe.rawl, FUN = "mean")
aggregate(look ~ experiment + type , data = doe.rawl, FUN = "sd")
aggregate(total_hab ~ experiment, data = doe.wide, FUN = "mean")
aggregate(total_hab ~ experiment, data = doe.wide, FUN = "sd")

## STEP 3. Perform analyses
# set treatment contrasts
options(contrasts = c("contr.treatment", "contr.poly"))

# Paired T-tests on Log looks -----------------------
# Experiment 1
t1 <- t.test(loglook ~ type, data = dplyr::filter(doe.rawl, experiment == "Exp.1"), paired = TRUE, var.equal = FALSE); t1
cohen.d(filter(doe.rawl, type == "High" & experiment == "Exp.1")$loglook, filter(doe.rawl, type == "Low"& experiment == "Exp.1")$loglook, paired = TRUE, hedges.correction = FALSE)
pwr.t.test(n = 20, d = .695, type = "paired", sig.level = 0.05, power = NULL)

# Experiment 2
t2 <- t.test(loglook ~ type, data = dplyr::filter(doe.rawl, experiment == "Exp.2"), paired = TRUE); t2.raw
cohen.d(filter(doe.rawl, type == "high_avg" & experiment == "Exp.2")$loglook, filter(doe.rawl, type == "low_avg"& experiment == "Exp.2")$loglook, paired = TRUE, hedges.correction = FALSE)
pwr.t.test(n = 20, d = .104, type = "paired", sig.level = 0.05, power = NULL)

# Experiment 3
t3.raw <- t.test(loglook ~ type, data = dplyr::filter(doe.rawl, experiment == "Exp.3"), paired = TRUE); t3.raw
t3.raw$p.value/2
cohen.d(filter(doe.rawl, type == "high_avg" & experiment == "Exp.3")$look, filter(doe.rawl, type == "low_avg"& experiment == "Exp.3")$look, paired = TRUE, hedges.correction = FALSE)
pwr.t.test(n = 20, d = 0.633, type = "paired", sig.level = 0.05, power = NULL)


# One Sample T-tests on Proportion Looking (Supplemental Analyses): do infants look at the high jump more than they would by chance = 0.5?-----------
aggregate(high_prop_avg ~ experiment, data = doe.wide, FUN = "mean")
aggregate(high_prop_avg ~ experiment, data = doe.wide, FUN = "sd")

# Experiment 1
t1.prop <- t.test(x = dplyr::filter(doe.wide, experiment == "Exp.1")$high_prop_avg, mu = 0.5); t1.prop; t_out(t1.prop)

# Experiment 2
t2.prop <- t.test(x = dplyr::filter(doe.wide, experiment == "Exp.2")$high_prop_avg, mu = 0.5); t2.prop; t_out(t2.prop)

# Experiment 3
t3.prop <- t.test(x = dplyr::filter(doe.wide, experiment == "Exp.3")$high_prop_avg, mu = 0.5); t3.prop; t_out(t3.prop)


# Non-parametric equivalent of t-test on untransformed LTs (Supplemental Analyses) --------------
# Experiment 1
wilcox.test(look ~ type,
            data = dplyr::filter(doe.raw, experiment == "Exp.1"),
            paired = TRUE,
            conf.int = TRUE)

# Experiment 2
wilcox.test(look ~ type,
            data = dplyr::filter(doe.raw, experiment == "Exp.2"),
            paired = TRUE,
            conf.int = TRUE)

# Experiment 3
wilcox.test(look ~ type,
            data = dplyr::filter(doe.raw, experiment == "Exp.3"),
            paired = TRUE,
            conf.int = TRUE)

# Experiment 4
wilcox.test(look ~ type,
            data = dplyr::filter(doe.raw, experiment == "Exp.4"),
            paired = TRUE,
            conf.int = TRUE)

# Experiment 5
wilcox.test(look ~ type,
            data = dplyr::filter(doe.rawl, experiment == "Exp.5"),
            paired = TRUE,
            conf.int = TRUE)

# Linear mixed effects models on log-transformed LTs

# Establish preferred reference groups for factors
doe.rawl$type <- relevel(doe.rawl$type, "Low")
doe.rawl$order <- relevel(doe.rawl$order, "low")
doe.rawl$sex <- relevel(doe.rawl$sex, "m")

# Experiment 1 --------------------------

# null model
model10 <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1"),
                          formula = loglook ~ 1 + (1|subj), 
                          REML=FALSE)
summary(model10)

qqnorm(resid(model10)) # checking for normality of residuals

# hypothesis driven model
model1a <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1"),
                          formula = loglook ~ type + (1|subj), 
                          REML=FALSE)
summary(model1a)
confint(model1a)

plot(allEffects(model1a))
qqnorm(resid(model1a)) # checking for normality of residuals

# standardized betas
model1a.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1"),
                               formula = scale(loglook) ~ type + (1|subj), 
                               REML=FALSE)
fixef(model1a.beta)

# LRT to compare model fit
anova(model10, model1a)

# plotting all subjects by Cook's Distance
plot(influence(model1a, "subj"), which="cook",
     cutoff=4/20, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# get D values
cooks.distance.estex(influence(model1a, "subj"))

# refit hypothesis driven model excluding influential vases
model1a.cooks <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1" & subj != "E1_11"),
                                formula = loglook ~ type + (1|subj), 
                                REML=FALSE)
summary(model1a.cooks)
confint(model1a.cooks)

# standardized values
model1a.cooks.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1" & subj != "E1_11"),
                                     formula = scale(loglook) ~ type + (1|subj), 
                                     REML=FALSE)
fixef(model1a.cooks.beta)

# exploratory model testing for order effects, and effect of sex
model1b <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1"),
                          formula = loglook ~ type * order + sex + (1|subj), 
                          REML=FALSE)
summary(model1b)
confint(model1b)

plot(allEffects(model1b))
qqnorm(resid(model1b)) # checking for normality of residuals


# standardized values
model1b.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1"),
                               formula = scale(loglook) ~ type * order + sex + (1|subj), 
                               REML=FALSE)
fixef(model1b.beta)

# LRT to compare model fit
anova(model1a, model1b)

# plot all subjects by Cook's Distance - no influential cases
plot(influence(model1b, "subj"), which="cook",
     cutoff=4/20, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")


# Experiment 2 --------------------------

# null model
model20 <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.2"),
                          formula = loglook ~ 1 + (1|subj), 
                          REML=FALSE)
summary(model20)
qqnorm(resid(model20)) # checking for normality of residuals


# hypothesis-driven model
model2a <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.2"),
                          formula = loglook ~ type + (1|subj), 
                          REML=FALSE)
summary(model2a)
confint(model2a)

plot(allEffects(model2a))
qqnorm(resid(model2a)) # checking for normality of residuals



# LRT to compare model fit
anova(model20, model2a)

# standardized values
model2a.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.2"),
                               formula = scale(loglook) ~ type + (1|subj), 
                               REML=FALSE)
fixef(model2a.beta)

# plot all subjects by Cook's Distance
plot(influence(model2a, "subj"), which="cook",
     cutoff=4/20, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# get values for D
cooks.distance.estex(influence(model2a, "subj"))

# refit hypothesis driven model without influential subjects
model2a.cooks <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.2" & subj != "E2_5"),
                                formula = loglook ~ type + (1|subj), 
                                REML=FALSE)
summary(model2a.cooks)
confint(model2a.cooks)

# standardized values
model2a.cooks.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.2" & subj != "E2_5"),
                                     formula = scale(loglook) ~ type + (1|subj), 
                                     REML=FALSE)
summary(model2a.cooks.beta)
confint(model2a.cooks.beta)

# exploratory model looking for test event order effects and sex
model2b <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.2"),
                          formula = loglook ~ type * order + sex + (1|subj), 
                          REML=FALSE)
summary(model2b)
confint(model2b)

plot(allEffects(model2b))
qqnorm(resid(model2b)) # checking for normality of residuals


# plot all subjects by Cook's Distance
plot(influence(model2b, "subj"), which="cook",
     cutoff=4/20, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model2b, "subj"))

# refit exploratory model without influential cases
model2b.cooks <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.2" & subj != "E2_5"),
                                formula = loglook ~ type * order + sex + (1|subj), 
                                REML=FALSE)
summary(model2b.cooks)

# calculate M and SD for looking to each type of test event within order
aggregate(look ~ type + order, data = dplyr::filter(doe.raw, experiment == "Exp.2"), FUN = "mean")
aggregate(look ~ type + order, data = dplyr::filter(doe.raw, experiment == "Exp.2"), FUN = "sd")


# calculate M and SD for looking to each type of test event within order, without influential case
aggregate(data = dplyr::filter(doe.rawl, experiment == "Exp.2" & subj != "E2_5"), look ~ order + type, FUN=mean)
aggregate(data = dplyr::filter(doe.rawl, experiment == "Exp.2" & subj != "E2_5"), look ~ order + type, FUN=sd)

# get effects for type within each order
eff2 <- allEffects(model2b.cooks)
eff_df2 <- as.data.frame(eff2[["type*order"]])
eff_df2
m2lsm <- lsmeans(model2b.cooks, specs = ~order*type)    # at = list(cond = factor(0))
contrast(m2lsm, method = "pairwise", adjust="none")    # can use p.adjust 
confint(contrast(m2lsm, method = "pairwise", adjust="none"))

# standardized values
model2b.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.2"),
                               formula = scale(loglook) ~ type * order + sex + (1|subj), 
                               REML=FALSE)
fixef(model2b.beta)


# use LRT to compare model fit
anova(model2a,model2b)


# COMPARING EXP 1 and EXP 2 --------------------------
model12 <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1" | experiment == "Exp.2"),
                          formula = loglook ~ type * experiment + (1|subj), 
                          REML=FALSE)
summary(model12)
confint(model12)

plot(allEffects(model12))
qqnorm(resid(model12)) # checking for normality of residuals


# get standardized values
model12.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1" | experiment == "Exp.2"),
                               formula = scale(loglook) ~ type * experiment + (1|subj), 
                               REML=FALSE)
summary(model12.beta)
confint(model12.beta)

# plot all subjects by Cook's Distance
plot(influence(model12, "subj"), which="cook",
     cutoff=4/40, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# get values for Cook's Distance
cooks.distance.estex(influence(model12, "subj"))

# refit model12 removing influential cases
model12.cooks <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1" | experiment == "Exp.2" & subj != "E2_5"),
                                formula = loglook ~ type * experiment + (1|subj), 
                                REML=FALSE)
summary(model12.cooks)
confint(model12.cooks)

# get standardized values
model12.cooks.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.1" | experiment == "Exp.2" &subj != "E2_5"),
                                     formula = scale(loglook) ~ type * experiment + (1|subj), 
                                     REML=FALSE)
summary(model12.cooks.beta)


# Experiment 3 --------------------------

# null model
model30 <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3"),
                          formula = loglook ~ 1 + (1|subj), 
                          REML=FALSE)
summary(model30)

qqnorm(resid(model30)) # checking for normality of residuals


# hypothesis driven model
model3a <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3"),
                          formula = loglook ~ type + (1|subj), 
                          REML=FALSE)
summary(model3a)
confint(model3a)

plot(allEffects(model3a))
qqnorm(resid(model3a)) # checking for normality of residuals

# standardized values
model3a.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3"),
                               formula = scale(loglook) ~ type + (1|subj), 
                               REML=FALSE)
fixef(model3a.beta)

# plot all subjects by Cook's D
plot(influence(model3a, "subj"), which="cook",
     cutoff=4/20, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# get values for D
cooks.distance.estex(influence(model3a, "subj"))

# refit hypothesis driven model after removing influential cases
model3a.cooks <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" & subj != "E3_19"),
                                formula = loglook ~ type + (1|subj), 
                                REML=FALSE)
summary(model3a.cooks)
confint(model3a.cooks)

# standardized values
model3a.cooks.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" & subj != "E3_19"),
                                     formula = scale(loglook) ~ type + (1|subj), 
                                     REML=FALSE)
summary(model3a.cooks.beta)

# use LRT to compare model fit
anova(model30, model3a)

# exploratory model testing for effects of test event order presentation and sex
model3b <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3"),
                          formula = loglook ~ type * order + sex + (1|subj), 
                          REML=FALSE)
summary(model3b)
confint(model3b)

plot(allEffects(model3b))
qqnorm(resid(model3b)) # checking for normality of residuals

# standardized values
model3b.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3"),
                               formula = scale(loglook) ~ type * order + sex + (1|subj), 
                               REML=FALSE)
summary(model3b.beta)

# plot all subjects by Cook's D
plot(influence(model3b, "subj"), which="cook",
     cutoff=4/20, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# get values for D
cooks.distance.estex(influence(model3b, "subj"))

# refit exploratory model removing influential cases
model3b.cook <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" & subj != "E3_19"),
                               formula = loglook ~ type * order + sex + (1|subj), 
                               REML=FALSE)
summary(model3b.cook)

# use LRT to compare model fit
anova(model3a, model3b)


# COMPARING EXP 1 and EXP 3 --------------------------
model13 <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" | experiment == "Exp.1"),
                          formula = loglook ~ type * experiment + (1|subj), 
                          REML=FALSE)
summary(model13)
confint(model13)

plot(allEffects(model13))
qqnorm(resid(model13)) # checking for normality of residuals


# standardized values
model13.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" | experiment == "Exp.1"),
                               formula = scale(loglook) ~ type * experiment + (1|subj), 
                               REML=FALSE)
summary(model13.beta)

# plot all subjects by Cook's D
plot(influence(model13, "subj"), which="cook",
     cutoff=4/40, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# get values for Cook's D
cooks.distance.estex(influence(model13, "subj"))

# refit model13 after excluding influential subjects
model13.cooks <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" | experiment == "Exp.1" & subj != "E1_7" & subj != "E1_11"),
                                formula = loglook ~ type * experiment + (1|subj), 
                                REML=FALSE)
summary(model13.cooks)
confint(model13.cooks)

# standardized values
model13.cooks.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" | experiment == "Exp.1" & subj != "E1_7" & subj != "E1_11"),
                                     formula = scale(loglook) ~ type * experiment + (1|subj), 
                                     REML=FALSE)
summary(model13.cooks.beta)

# afer finding no interaction, remove the interaction
model13b <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" | experiment == "Exp.1"),
                           formula = loglook ~ type + experiment + (1|subj), 
                           REML=FALSE)
summary(model13b)
confint(model13b)

plot(allEffects(model13b))
qqnorm(resid(model13b)) # checking for normality of residuals


# standardized values
model13b.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" | experiment == "Exp.1"),
                                formula = scale(look) ~ type + experiment + (1|subj), 
                                REML=FALSE)
summary(model13b.beta)

# plot all subjects by Cook's D
plot(influence(model13b, "subj"), which="cook",
     cutoff=4/40, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# generate values for Cook's D
cooks.distance.estex(influence(model13b, "subj"))

# refit model13b excluding influential cases
model13b.cooks <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" | experiment == "Exp.1" & subj != "E1_12" & subj != "E1_11"),
                                 formula = loglook ~ type + experiment + (1|subj), 
                                 REML=FALSE)
summary(model13b.cooks)
confint(model13b.cooks)

# standardized values
model13b.cooks.beta <- lmerTest::lmer(data = dplyr::filter(doe.rawl, experiment == "Exp.3" | experiment == "Exp.1" & subj != "E1_12" & subj != "E1_11"),
                                      formula = scale(loglook) ~ type + experiment + (1|subj), 
                                      REML=FALSE)
summary(model13b.cooks)
confint(model13b.cooks)

# compare fit using LRT
anova(model13, model13b)

# COMPARING ALL EXPERIMENTS (1 to 3) --------------------------

# Are there differences in attention at hab across the 3 studies?
model.all.hab <- lm(data = dplyr::filter(doe.wide),
                formula = total_hab ~ experiment)
summary(model.all.hab)
TukeyHSD(aov(model.all.hab))

plot(allEffects(model.all.hab))
qqnorm(resid(model.all.hab)) # checking for normality of residuals


# find M and SD for hab attention
aggregate(total_hab ~ experiment, data = doe.rawl, FUN = "mean")
aggregate(total_hab ~ experiment, data = doe.rawl, FUN = "sd")

# Are there differences in attention at test across the 3 studies?
model.all.test <- lmer(data = doe.rawl,
                    formula = loglook ~ experiment + (1|subj),
                    REML = FALSE)
summary(model.all.test)

plot(allEffects(model.all.test))
qqnorm(resid(model.all.test)) # checking for normality of residuals

# comparing experiments pairwise
m2lsm <- lsmeans(model.all.test, specs = ~experiment)    # at = list(cond = factor(0))
contrast(m2lsm, method = "pairwise", adjust="tukey")    # can use p.adjust 
confint(contrast(m2lsm, method = "pairwise", adjust="tukey"))

# find M and SD for test attention
aggregate(look ~ experiment, data = doe.rawl, FUN = "mean")
aggregate(look ~ experiment, data = doe.rawl, FUN = "sd")


# Calculating Reliability -----------------------------------------------
reliability <- read.csv(file = "doe_reliability_osf_6dec16.csv", header = TRUE)
study1rel <- dplyr::filter(reliability, experiment == "Exp.1")
study2rel <- dplyr::filter(reliability, experiment == "Exp.2")
study3rel <- dplyr::filter(reliability, experiment == "Exp.3")

icc(study1rel[,4:5], model = "twoway", type = "agreement", unit = "single"); mean(study1rel$agree) # Study 1: ICC = 0.969, agreement = 94.17%
icc(study2rel[,4:5], model = "twoway", type = "agreement", unit = "single"); mean(study2rel$agree) # Study 2: ICC = 0.993, agreement = 95.83%
icc(study3rel[,4:5], model = "twoway", type = "agreement", unit = "single"); mean(study3rel$agree) # Study 3: ICC = 0.972, agreement = 94.17%
