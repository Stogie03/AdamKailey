#***************************************#
#** Kaggle Animal Shelter Competition **#
#**                                   **#
#** Knowledge Competition             **#
#** Test and Training Data Sets       **#
#** Goal: Predict outcome of animals  **#
#**       leaving the animal center:  **#
#**       Adoption                    **#
#**       Died                        **#
#**       Euthanasia                  **#
#**       Return to Owner             **#
#**       Transfer                    **#
#***************************************#

#** Set Working Directory **#

setwd("~/Kaggle/AnimalShelterCompetition")


#** Import Test/Train/Sample Submission Data **#

sample = read.csv("sample_submission.csv.gz", stringsAsFactors=FALSE)
test   = read.csv("test.csv.gz", stringsAsFactors=FALSE)
train  = read.csv("train.csv.gz", stringsAsFactors=FALSE)

#**************************#
#** Data Prep            **#
#**                      **#
#** Transform Attributes **#
#** Prep for Model       **#
#**************************#

train2 = train

#** Convert age to days **#

train2age = data.frame(train2[, c("AnimalID", "AgeuponOutcome")], age = NA_integer_, stringsAsFactors = FALSE)
train2age[which(substr(train2age$AgeuponOutcome, 
                       nchar(train2age$AgeuponOutcome) - 2, 
                       nchar(train2age$AgeuponOutcome)) == "ars"), "age"] = as.numeric(substr(train2age[which(substr(train2age$AgeuponOutcome, 
                                                                                       nchar(train2age$AgeuponOutcome) - 2, 
                                                                                       nchar(train2age$AgeuponOutcome)) == "ars"), "AgeuponOutcome"], 1, 2)) * 365
train2age[which(substr(train2$AgeuponOutcome, 
                       nchar(train2$AgeuponOutcome) - 2, 
                       nchar(train2$AgeuponOutcome)) == "ths"), "age"] = as.numeric(substr(train2age[which(substr(train2age$AgeuponOutcome, 
                                                                                    nchar(train2age$AgeuponOutcome) - 2, 
                                                                                    nchar(train2age$AgeuponOutcome)) == "ths"), "AgeuponOutcome"], 1, 2)) * 30
train2age[which(substr(train2$AgeuponOutcome, 
                       nchar(train2$AgeuponOutcome) - 2, 
                       nchar(train2$AgeuponOutcome)) == "eks"), "age"] = as.numeric(substr(train2age[which(substr(train2age$AgeuponOutcome, 
                                                                                    nchar(train2age$AgeuponOutcome) - 2, 
                                                                                    nchar(train2age$AgeuponOutcome)) == "eks"), "AgeuponOutcome"], 1, 2)) * 7
train2age[which(substr(train2$AgeuponOutcome, 
                       nchar(train2$AgeuponOutcome) - 2, 
                       nchar(train2$AgeuponOutcome)) == "ays"), "age"] = as.numeric(substr(train2age[which(substr(train2age$AgeuponOutcome, 
                                                                                    nchar(train2age$AgeuponOutcome) - 2, 
                                                                                    nchar(train2age$AgeuponOutcome)) == "ays"), "AgeuponOutcome"], 1, 2)) * 1
train2age[which(substr(train2$AgeuponOutcome, 
                       nchar(train2$AgeuponOutcome) - 2, 
                       nchar(train2$AgeuponOutcome)) == "ear"), "age"] = as.numeric(substr(train2age[which(substr(train2age$AgeuponOutcome, 
                                                                                    nchar(train2age$AgeuponOutcome) - 2, 
                                                                                    nchar(train2age$AgeuponOutcome)) == "ear"), "AgeuponOutcome"], 1, 2)) * 365
train2age[which(substr(train2$AgeuponOutcome, 
                       nchar(train2$AgeuponOutcome) - 2, 
                       nchar(train2$AgeuponOutcome)) == "nth"), "age"] = as.numeric(substr(train2age[which(substr(train2age$AgeuponOutcome, 
                                                                                    nchar(train2age$AgeuponOutcome) - 2, 
                                                                                    nchar(train2age$AgeuponOutcome)) == "nth"), "AgeuponOutcome"], 1, 2)) * 30
train2age[which(substr(train2$AgeuponOutcome, 
                       nchar(train2$AgeuponOutcome) - 2, 
                       nchar(train2$AgeuponOutcome)) == "eek"), "age"] = as.numeric(substr(train2age[which(substr(train2age$AgeuponOutcome, 
                                                                                    nchar(train2age$AgeuponOutcome) - 2, 
                                                                                    nchar(train2age$AgeuponOutcome)) == "eek"), "AgeuponOutcome"], 1, 2)) * 7
train2age[which(substr(train2$AgeuponOutcome, 
                       nchar(train2$AgeuponOutcome) - 2, 
                       nchar(train2$AgeuponOutcome)) == "day"), "age"] = as.numeric(substr(train2age[which(substr(train2age$AgeuponOutcome, 
                                                                                    nchar(train2age$AgeuponOutcome) - 2, 
                                                                                    nchar(train2age$AgeuponOutcome)) == "day"), "AgeuponOutcome"], 1, 2)) * 1
temp = train2age[which(is.na(train2age$age) == TRUE),]
rm(temp)


#** Convert Sex to male/female column and intact/not intact column **#

train2sex = data.frame(train2[, c("AnimalID", "SexuponOutcome")], gender = NA_integer_, intact = NA_integer_, stringsAsFactors = FALSE)

train2sex[which(train2sex$SexuponOutcome %in% c("Neutered Male", "Intact Male")), "gender"] = 1
train2sex[which(train2sex$SexuponOutcome %in% c("Spayed Female", "Intact Female")), "gender"] = 0

train2sex[which(train2sex$SexuponOutcome %in% c("Neutered Male", "Spayed Female")), "intact"] = 0
train2sex[which(train2sex$SexuponOutcome %in% c("Intact Male", "Intact Female")), "intact"] = 1

temp = train2sex[which(is.na(train2sex$gender) == TRUE),]
temp = train2sex[which(is.na(train2sex$intact) == TRUE),]
rm(temp)


#** Convert Animal Type to Binary **#

train2type = data.frame(train2[, c("AnimalID", "AnimalType")], type = NA_integer_, stringsAsFactors = FALSE)

train2type[which(train2type$AnimalType == "Dog"), "type"] = 1
train2type[which(train2type$AnimalType == "Cat"), "type"] = 0


#** Convert Name to Binary **#

train2name = data.frame(train2[, c("AnimalID", "Name")], name = NA_integer_, stringsAsFactors = FALSE)

train2name[which(train2name$Name == ""), "name"] = 0
train2name[which(is.na(train2name$name) == TRUE), "name"] = 1



#** Convert date to quarters/months/dow/etc **#

train2date = data.frame(train2[, c("AnimalID", "DateTime")], mon = NA_integer_, 
                                                             tue = NA_integer_,
                                                             wed = NA_integer_,
                                                             thu = NA_integer_,
                                                             fri = NA_integer_,
                                                             sat = NA_integer_,
                                                             sun = NA_integer_,
                                                             weekend = NA_integer_, 
                                                             jan = NA_integer_,
                                                             feb = NA_integer_,
                                                             mar = NA_integer_,
                                                             apr = NA_integer_,
                                                             may = NA_integer_,
                                                             jun = NA_integer_,
                                                             jul = NA_integer_,
                                                             aug = NA_integer_,
                                                             sep = NA_integer_,
                                                             oct = NA_integer_,
                                                             nov = NA_integer_,
                                                             dec = NA_integer_,
                                                             q1 = NA_integer_,
                                                             q2 = NA_integer_,
                                                             q3 = NA_integer_,
                                                             q4 = NA_integer_,
                                                             morn = NA_integer_,
                                                             aft = NA_integer_,
                                                             night = NA_integer_,
                        stringsAsFactors = FALSE)

train2date[which(weekdays(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d')) == "Monday"), "mon"] = 1
train2date[which(is.na(train2date$mon) == TRUE), "mon"] = 0
train2date[which(weekdays(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d')) == "Tuesday"), "tue"] = 1
train2date[which(is.na(train2date$tue) == TRUE), "tue"] = 0
train2date[which(weekdays(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d')) == "Wednesday"), "wed"] = 1
train2date[which(is.na(train2date$wed) == TRUE), "wed"] = 0
train2date[which(weekdays(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d')) == "Thursday"), "thu"] = 1
train2date[which(is.na(train2date$thu) == TRUE), "thu"] = 0
train2date[which(weekdays(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d')) == "Friday"), "fri"] = 1
train2date[which(is.na(train2date$fri) == TRUE), "fri"] = 0
train2date[which(weekdays(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d')) == "Saturday"), "sat"] = 1
train2date[which(is.na(train2date$sat) == TRUE), "sat"] = 0
train2date[which(weekdays(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d')) == "Sunday"), "sun"] = 1
train2date[which(is.na(train2date$sun) == TRUE), "sun"] = 0

train2date[which(weekdays(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d')) %in% c("Saturday", "Sunday")), "weekend"] = 1
train2date[which(is.na(train2date$weekend) == TRUE), "weekend"] = 0

train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "January"), "jan"] = 1
train2date[which(is.na(train2date$jan) == TRUE), "jan"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "February"), "feb"] = 1
train2date[which(is.na(train2date$feb) == TRUE), "feb"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "March"), "mar"] = 1
train2date[which(is.na(train2date$mar) == TRUE), "mar"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "April"), "apr"] = 1
train2date[which(is.na(train2date$apr) == TRUE), "apr"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "May"), "may"] = 1
train2date[which(is.na(train2date$may) == TRUE), "may"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "June"), "jun"] = 1
train2date[which(is.na(train2date$jun) == TRUE), "jun"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "July"), "jul"] = 1
train2date[which(is.na(train2date$jul) == TRUE), "jul"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "August"), "aug"] = 1
train2date[which(is.na(train2date$aug) == TRUE), "aug"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "September"), "sep"] = 1
train2date[which(is.na(train2date$sep) == TRUE), "sep"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "October"), "oct"] = 1
train2date[which(is.na(train2date$oct) == TRUE), "oct"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "November"), "nov"] = 1
train2date[which(is.na(train2date$nov) == TRUE), "nov"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') == "December"), "dec"] = 1
train2date[which(is.na(train2date$dec)== TRUE), "dec"] = 0

train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') %in% c("January", "February", "March")), "q1"] = 1
train2date[which(is.na(train2date$q1)== TRUE), "q1"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') %in% c("April", "May", "June")), "q2"] = 1
train2date[which(is.na(train2date$q2)== TRUE), "q2"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') %in% c("July", "August", "September")), "q3"] = 1
train2date[which(is.na(train2date$q3)== TRUE), "q3"] = 0
train2date[which(format(as.Date(substr(train2date$DateTime,1,10), '%Y-%m-%d'), '%B') %in% c("October", "November", "December")), "q4"] = 1
train2date[which(is.na(train2date$q4)== TRUE), "q4"] = 0

train2date[which(as.numeric(substr(train2date$DateTime,12,13)) >= 0 & as.numeric(substr(train2date$DateTime,12,13)) <= 11), "morn"] = 1
train2date[which(is.na(train2date$morn)== TRUE), "morn"] = 0
train2date[which(as.numeric(substr(train2date$DateTime,12,13)) >= 12 & as.numeric(substr(train2date$DateTime,12,13)) <= 18), "aft"] = 1
train2date[which(is.na(train2date$aft)== TRUE), "aft"] = 0
train2date[which(as.numeric(substr(train2date$DateTime,12,13)) >= 19 & as.numeric(substr(train2date$DateTime,12,13)) <= 23), "night"] = 1
train2date[which(is.na(train2date$night)== TRUE), "night"] = 0




#** Convert Breed to Buckets **#





#** Convert Color to Buckets **#













#**************************#
#** Fill in Missing Data **#
#**************************#



#** Playing with Distributions **#


temp = data.frame(unique(train2[,c("Breed", "Color")]))



temp = merge(train2age[, c("AnimalID", "age")], train2sex[, c("AnimalID", "gender", "intact")], by = "AnimalID", all = TRUE)
temp = merge(temp, train2[,c("AnimalID", "AnimalType")], by = "AnimalID", all = TRUE)
agemax = 7000
temp = temp[which(temp$age < agemax),]
temp2 = aggregate(AnimalID ~ age + gender + AnimalType, data = temp, length)
temp3 = aggregate(intact ~ age + gender + AnimalType, data = temp, sum)
temp = merge(temp2, temp3, by = c("age", "gender", "AnimalType"), all = TRUE)
temp$intact = temp$intact/temp$AnimalID*100
temp = temp[order(temp$age, temp$gender),]
plot(temp[which(temp$gender == 0 & temp$AnimalType == "Dog"), "age"], temp[which(temp$gender == 0 & temp$AnimalType == "Dog"), "intact"], type = "l", col = "purple", xlab = "Age", ylab = "Percent Intact", xlim = c(0,agemax), ylim = c(0,110))
par(new=T)
plot(temp[which(temp$gender == 1 & temp$AnimalType == "Dog"), "age"], temp[which(temp$gender == 1 & temp$AnimalType == "Dog"), "intact"], type = "l", col = "blue", xlab = "Age", ylab = "Percent Intact", xlim = c(0,agemax), ylim = c(0,110))
par(new = T)
plot(temp[which(temp$gender == 0 & temp$AnimalType == "Cat"), "age"], temp[which(temp$gender == 0 & temp$AnimalType == "Cat"), "intact"], type = "l", col = "pink", xlab = "Age", ylab = "Percent Intact", xlim = c(0,agemax), ylim = c(0,110))
par(new=T)
plot(temp[which(temp$gender == 1 & temp$AnimalType == "Cat"), "age"], temp[which(temp$gender == 1 & temp$AnimalType == "Cat"), "intact"], type = "l", col = "orange", xlab = "Age", ylab = "Percent Intact", xlim = c(0,agemax), ylim = c(0,110))






breed = data.frame(unique(train2[, c("AnimalID", "Breed")]))
breed2 = aggregate(AnimalID ~ Breed, data = breed, length)
breed2 = breed2[order(breed2$AnimalID),]
