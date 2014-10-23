# October 23rd 2014
# ZOL851  In Class exercise for resampling.

# It is often the case that a genetic or environemntal (i.e. temperature or nutritional) stressor/perturbation will not only influence the mean value of a trait, but the variance (both environmental and genetic) for the trait as well. There is a considerable literature that examines how and why this occurs. However, approropraite measures to assess variation are somewhat trickier than for estimating "mean" effect sizes. 

# A commonly used approach is to use the coefficient of variation
CV <- function(x) {sd(x)/mean(x)}

# However while the intepretation of CV is pretty straight forward, statistical inference for it is not. (There are in fact better approaches based on Levene's statistic among others).

# For the dll.data dataset I want you to assess how much if any additional variance (as measured using CV) the mutants have compared with the wild types. Pick one line (only one) AND one temperture (25 or 30).

# I suggest calculating CV for each combination of line and genotype (we will only use data at one temperature and ignore tarsus length for this)

 

dll.data = read.csv("http://datadryad.org/bitstream/handle/10255/dryad.8377/dll.csv", header=TRUE)
 
dll.data = na.omit(dll.data)
dll.data$temp <- factor(dll.data$temp)
dll.data$replicate <- factor(dll.data$replicate)
dll.data$genotype <-relevel(dll.data$genotype, ref="wt")
dll.data$tarsus.scaled <- scale(dll.data$tarsus, center=TRUE, scale=FALSE)

with(dll.data, table(genotype, line, temp))

#1 - pick one line and temperature combination and generate a data subset to use. You should try to use something with reasonable sample sizes for each group.
dll.sub <- subset(dll.data, dll.data$line=="line-18" & dll.data$temp=="30")
with(dll.sub, table(genotype, line, temp))

dll.sub_wt <- subset(dll.sub, dll.sub$genotype=="wt")
dll.sub_Dl <- subset(dll.sub, dll.sub$genotype=="Dll")
#2 Compute the CV for each group.
CV_wt <- CV(dll.sub_wt$SCT)
CV_wt
CV_Dll <- CV(dll.sub_Dl$SCT)
CV_Dll

#3 Use a np bootstrap to compare CV between each group.
BootstrapCV <- function(x){
  x.boot <- sample(x, size=length(x), replace=T)
  CV(x.boot) }

boot.wt <- replicate(10000, BootstrapCV(dll.sub_wt$SCT))
boot.Dll <- replicate(10000, BootstrapCV(dll.sub_Dl$SCT))
hist(boot.wt, border="blue", xlim = c(min(boot.wt), max(boot.Dll)))
hist(boot.Dll, border="red", add=T)

quantile(boot.wt, probs = c(0.025, 0.975))
quantile(boot.Dll, probs = c(0.025, 0.975))

#4 Can you think of a way to implement a permutation test for this?

# 5 clearly we want to test this for the whole dataset. How might you approach this same question to address it for all lines? Write down steps.



#### Note a more common approach is to use Levene's test as you can deviates among individuals (see Dworkin 2005)
### Below I have written a general purpose function to generate Levene's deviates
LeveneDeviates <- function(y, group, med=TRUE, log_trans=TRUE) {
    
    #log transform data?
    if (log_trans==TRUE)
        y = log(y)
    
    # allows for use of mean or median as measure of central tendency
    if (med==TRUE)
        meds <- tapply(y, group, median, na.rm=TRUE)
    else 
        meds <- tapply(y, group, mean, na.rm=TRUE) 
    
    # calculates deviates for each observation from a measure of central tendency for a "group"
    abs(y - meds[group])}
    
lev_stat <- with(dll.data, 
    LeveneDeviates(y = SCT, group = genotype:temp:line, med=TRUE, log_trans=FALSE))

ls.lm <- lm(lev_stat ~ genotype*temp*line, data=dll.data)
anova(ls.lm)  # of course this ANOVA is not particularly valid given the lack of normality, use bootstrapping or permutation.


