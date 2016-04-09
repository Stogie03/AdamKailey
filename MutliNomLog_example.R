########################################################
#This is a test script to try and understand Multinomial Logistic Regression
#PACKAGES USED:
#   foreign
#   nnet
#   ggplot2
#   reshape2
################################################

ml <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")


#Descriptive Statistics on data
with(ml, table(ses, prog))
with(ml, do.call(rbind, tapply(write, prog,
                               function(x) c(M = mean(x), SD = sd(x)))))
#Logistic Regression
ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)

#predicted Probabilities
head(pp <- fitted(test))
pp2 <- fitted(test)
