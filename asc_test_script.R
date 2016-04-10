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

#** User Parameters **#

bcount = 200     # minimum occurrences of breed within training set for breed to become an independent variable
ccount = 200     # minimum occurrences of color within training set for color to become an independent variable

#**************************#
#** Data Prep            **#
#**                      **#
#** Transform Attributes **#
#** Prep for Model       **#
#**************************#

train2 = train


#** Recode the dependent variable for Stokesy **#

train2dep = data.frame(train2[, c("AnimalID", "OutcomeType")], stringsAsFactors = FALSE)
class = data.frame(OutcomeType = unique(train2dep$OutcomeType), dependent = 0:(length(unique(train2dep$OutcomeType)) - 1), stringsAsFactors = FALSE)
train2dep = merge(train2dep, class, by = "OutcomeType", all.x = TRUE)



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

train2breed = data.frame(train2[, c("AnimalID", "Breed")], 
                         mix = substr(train2$Breed, 1, unlist(gregexpr('Mix', train2$Breed)) - 2),
                         parenta = substr(train2$Breed, 1, do.call('rbind', gregexpr('/', train2$Breed))-1),
                         parentb = NA_character_,
                         parentc = NA_character_,
                         pure = NA_integer_,
                         stringsAsFactors = FALSE)

train2breed[which(do.call('rbind', list(regexpr('/', train2breed$Breed))) >= 0), "parentb"] = substr(train2breed[which(do.call('rbind', list(regexpr('/', train2breed$Breed))) >= 0), "Breed"],
                                                                                                do.call('rbind', list(regexpr('/', train2breed[which(do.call('rbind', list(regexpr('/', train2breed$Breed))) >= 0), "Breed"])))+1,
                                                                                                nchar(train2breed[which(do.call('rbind', list(regexpr('/', train2breed$Breed))) >= 0), "Breed"]))

train2breed[which(do.call('rbind', list(regexpr('/', train2breed$parentb))) >= 0), "parentc"] = substr(train2breed[which(do.call('rbind', list(regexpr('/', train2breed$parentb))) >= 0), "parentb"], 
                                                                                                 do.call('rbind', list(regexpr('/', train2breed[which(do.call('rbind', list(regexpr('/', train2breed$parentb))) >= 0), "parentb"])))+1, 
                                                                                                 nchar(train2breed[which(do.call('rbind', list(regexpr('/', train2breed$parentb))) >= 0), "parentb"]))
train2breed[which(do.call('rbind', list(regexpr('/', train2breed$parentb))) >= 0), "parentb"] = substr(train2breed[which(do.call('rbind', list(regexpr('/', train2breed$parentb))) >= 0), "parentb"],
                                                                                                  1,
                                                                                                  do.call('rbind', list(regexpr('/', train2breed[which(do.call('rbind', list(regexpr('/', train2breed$parentb))) >= 0), "parentb"])))-1)
train2breed[which(train2breed$mix == ""), "mix"] = NA
train2breed[which(train2breed$parenta == ""), "parenta"] = NA

temp = train2breed[which(is.na(train2breed$parentc) == FALSE),]
temp2 = train2breed[which(is.na(train2breed$parenta) == FALSE),]
temp3 = train2breed[which(train2breed$parenta == "Black" | train2breed$parentb == "Black"),]
temp4 = train2breed[which(is.na(train2breed$mix) == FALSE & is.na(train2breed$parenta) == FALSE),]
temp5 = train2breed[which(is.na(train2breed$parenta) == FALSE & is.na(train2breed$parentb) == TRUE),]

train2breed[which(train2breed$parenta == "Black"), "parentb"] = NA
train2breed[which(train2breed$parenta == "Black"), "parenta"] = "Back/Tan Hound"
train2breed[which(train2breed$parentb == "Black"), "parentc"] = NA
train2breed[which(train2breed$parentb == "Black"), "parentb"] = "Back/Tan Hound"
train2breed[which(is.na(train2breed$parentb) == TRUE & is.na(train2breed$parentc) == FALSE), "parentb"] = train2breed[which(is.na(train2breed$parentb) == TRUE & is.na(train2breed$parentc) == FALSE), "parentc"]
train2breed[which(is.na(train2breed$parentb) == FALSE & train2breed$parentb == train2breed$parentc), "parentc"] = NA
train2breed[which(is.na(train2breed$parenta) == FALSE & is.na(train2breed$parentb) == TRUE), "parenta"] = NA

train2breed = train2breed[,c("AnimalID", "Breed", "mix", "parenta", "parentb", "pure")]

train2breed[which(is.na(train2breed$mix) == TRUE & is.na(train2breed$parenta) == TRUE & is.na(train2breed$parentb) == TRUE), "pure"] = 1
train2breed[which(is.na(train2breed$pure) == TRUE), "pure"] = 0
train2breed[which(train2breed$Breed %in% c("Domestic Shorthair", "Domestic Longhair", "Domestic Medium Hair")), "pure"] = 0
temp = train2breed[which(train2breed$pure == 1),]
temp = data.frame(unique(temp[,"Breed"]))


bbpure = aggregate(pure ~ Breed, data = train2breed[which(train2breed$pure == 1),], sum)
bbmix  = aggregate(AnimalID ~ mix, data = train2breed[which(is.na(train2breed$mix) == FALSE),], length)
colnames(bbmix) = c("Breed", "mix")
bbpa = aggregate(AnimalID ~ parenta, data = train2breed[which(is.na(train2breed$parenta) == FALSE & is.na(train2breed$parentb) == FALSE),], length)
colnames(bbpa) = c("Breed", "parent2")
bbpb = aggregate(AnimalID ~ parentb, data = train2breed[which(is.na(train2breed$parenta) == FALSE & is.na(train2breed$parentb) == FALSE),], length)
colnames(bbpb) = c("Breed", "parent2")
bbpab = aggregate(parent2 ~ Breed, data = data.frame(rbind(bbpa, bbpb), stringsAsFactors = FALSE), sum)
rm(bbpa, bbpb)
bbdom = aggregate(AnimalID ~ Breed, data = train2breed[which(train2breed$Breed %in% c("Domestic Shorthair", "Domestic Longhair", "Domestic Medium Hair")),], length)
colnames(bbdom) = c("Breed", "domestic")

bb1 = merge(bbpure, bbpab, by = "Breed", all = TRUE)
bb2 = merge(bb1, bbmix, by = "Breed", all = TRUE)
bb3 = merge(bb2, bbdom, by = "Breed", all = TRUE)

bb3[which(is.na(bb3$pure) == TRUE), "pure"] = 0
bb3[which(is.na(bb3$parent2) == TRUE), "parent2"] = 0
bb3[which(is.na(bb3$mix) == TRUE), "mix"] = 0
bb3[which(is.na(bb3$domestic) == TRUE), "domestic"] = 0

temp = aggregate(cbind(pure, parent2, mix, domestic) ~ 1, data = bb3, sum)


bb4 = data.frame(Breed = bb3$Breed, bcount = bb3$pure + bb3$parent2 + bb3$mix + bb3$domestic, stringsAsFactors = FALSE)
bb4 = bb4[order(bb4$bcount, decreasing = TRUE),]
bbclass = data.frame(Breed = c(bb4[which(bb4$bcount >= bcount), "Breed"], "OtherBreed"), bclass = NA_character_, stringsAsFactors = FALSE)
bbclass$bclass = gsub(" ", "", bbclass$Breed)

train2breed = data.frame(train2breed, matrix(NA_integer_, ncol = length(bbclass$bclass), nrow = length(train2breed$AnimalID)), stringsAsFactors = FALSE)
colnames(train2breed)[(ncol(train2breed) - length(bbclass$Breed) + 1):ncol(train2breed)] = bbclass$bclass

for (i in (ncol(train2breed) - length(bbclass$Breed) + 1):ncol(train2breed))
{
  train2breed[which(names(train2breed)[i] == gsub(" ", "", train2breed$Breed)), i] = 1
  train2breed[which(names(train2breed)[i] == gsub(" ", "", train2breed$mix)), i] = 1
  train2breed[which(names(train2breed)[i] == gsub(" ", "", train2breed$parenta)), i] = 1
  train2breed[which(names(train2breed)[i] == gsub(" ", "", train2breed$parentb)), i] = 1
}
for (i in (ncol(train2breed) - length(bbclass$Breed) + 1):ncol(train2breed))
{
  train2breed[which(is.na(train2breed[,i]) == TRUE),i] = 0
}

train2breed[which(apply(train2breed[,(ncol(train2breed) - length(bbclass$Breed) + 1):ncol(train2breed)], 1, sum) == 0), "OtherBreed"] = 1


rm(temp, temp2, temp3, temp4, temp5, bb1, bb2, bbdom, bbmix, bbpab, bbpure, i, bb3, bb4, bbclass)






#** Convert Color to Buckets **#

train2color = data.frame(train2[, c("AnimalID", "AnimalType", "Color")], 
                         color1 = substr(train2$Color, 1, do.call('rbind', gregexpr('/', train2$Color)) - 1),
                         color2 = NA_character_,
                         solid = NA_integer_,
                         brindle = NA_integer_,
                         tabby = NA_integer_,
                         tricolor = NA_integer_,
                         point = NA_integer_,
                         merle = NA_integer_,
                         stringsAsFactors = FALSE)

train2color[which(do.call('rbind', list(regexpr('/', train2color$Color))) >= 0), "color2"] = substr(train2color[which(do.call('rbind', list(regexpr('/', train2color$Color))) >= 0), "Color"],
                                                                                                     do.call('rbind', list(regexpr('/', train2color[which(do.call('rbind', list(regexpr('/', train2color$Color))) >= 0), "Color"])))+1,
                                                                                                     nchar(train2color[which(do.call('rbind', list(regexpr('/', train2color$Color))) >= 0), "Color"]))



train2color[which(train2color$color1 == ""), "solid"] = 1
train2color[which(is.na(train2color$solid) == TRUE), "solid"] = 0
train2color[which(do.call('rbind', list(regexpr('Tabby', train2color$Color))) >= 0), "solid"] = 0
train2color[which(do.call('rbind', list(regexpr('Brindle', train2color$Color))) >= 0), "solid"] = 0
train2color[which(do.call('rbind', list(regexpr('Tabby', train2color$Color))) >= 0), "tabby"] = 1
train2color[which(do.call('rbind', list(regexpr('Brindle', train2color$Color))) >= 0), "brindle"] = 1
train2color[which(is.na(train2color$tabby) == TRUE), "tabby"] = 0
train2color[which(is.na(train2color$brindle) == TRUE), "brindle"] = 0
train2color[which(do.call('rbind', list(regexpr('Tricolor', train2color$Color))) >= 0), "solid"] = 0
train2color[which(do.call('rbind', list(regexpr('Tricolor', train2color$Color))) >= 0), "tricolor"] = 1
train2color[which(is.na(train2color$tricolor) == TRUE), "tricolor"] = 0
train2color[which(do.call('rbind', list(regexpr('Point', train2color$Color))) >= 0), "solid"] = 0
train2color[which(do.call('rbind', list(regexpr('Point', train2color$Color))) >= 0), "point"] = 1
train2color[which(is.na(train2color$point) == TRUE), "point"] = 0
train2color[which(do.call('rbind', list(regexpr('Merle', train2color$Color))) >= 0), "solid"] = 0
train2color[which(do.call('rbind', list(regexpr('Merle', train2color$Color))) >= 0), "merle"] = 1
train2color[which(is.na(train2color$merle) == TRUE), "merle"] = 0

temp = train2color[which(train2color$solid == 1),]

cbsolid = aggregate(solid ~ Color, data = train2color[which(train2color$solid == 1),], sum)
cbc1 = aggregate(AnimalID ~ color1, data = train2color[which(train2color$color1 != ""),], length)
colnames(cbc1) = c("Color", "multi")
cbc2 = aggregate(AnimalID ~ color2, data = train2color[which(is.na(train2color$color2) == FALSE),], length)
colnames(cbc2) = c("Color", "multi")
cbmulti = aggregate(multi ~ Color, data = data.frame(rbind(cbc1, cbc2), stringsAsFactors = FALSE), sum)
cbbrindle = aggregate(brindle ~ Color, data = train2color[which(train2color$brindle == 1),], sum)
cbtabby = aggregate(tabby ~ Color, data = train2color[which(train2color$tabby == 1),], sum)
cbtricolor = aggregate(tricolor ~ Color, data = train2color[which(train2color$tricolor == 1),], sum)
cbpoint = aggregate(point ~ Color, data = train2color[which(train2color$point == 1),], sum)
cbmerle = aggregate(merle ~ Color, data = train2color[which(train2color$merle == 1),], sum)


cbsummary = data.frame(color = c(train2color[which(train2color$color1 != ""), "color1"],
                                 train2color[which(train2color$color1 != ""), "color2"],
                                 train2color[which(train2color$color1 == ""), "Color"]), 
                       flag  = 1, stringsAsFactors = FALSE)
cbsummary = aggregate(flag ~ color, data = cbsummary, sum)
cbsummary = cbsummary[order(cbsummary$flag, decreasing = TRUE),]


cbclass = data.frame(Color = c(cbsummary[which(cbsummary$flag >= ccount), "color"], "OtherColor"), cclass = NA_character_, stringsAsFactors = FALSE)
cbclass$cclass = gsub(" ", "", cbclass$Color)

train2color = data.frame(train2color, matrix(NA_integer_, ncol = length(cbclass$cclass), nrow = length(train2color$AnimalID)), stringsAsFactors = FALSE)
colnames(train2color)[(ncol(train2color) - length(cbclass$Color) + 1):ncol(train2color)] = cbclass$cclass

for (i in (ncol(train2color) - length(cbclass$Color) + 1):ncol(train2color))
{
  train2color[which(do.call('rbind', list(regexpr(names(train2color)[i], gsub(" ", "", train2color$Color)))) >= 0), i] = 1
}
for (i in (ncol(train2color) - length(cbclass$Color) + 1):ncol(train2color))
{
  train2color[which(is.na(train2color[,i]) == TRUE),i] = 0
}

train2color[which(apply(train2color[,(ncol(train2color) - length(cbclass$Color) + 1):ncol(train2color)], 1, sum) == 0), "OtherColor"] = 1


rm(cbbrindle, cbc1, cbc2, cbclass, cbmerle, cbmulti, cbpoint, cbsolid, cbsummary, cbtabby, cbtricolor, i)











#************************************#
#** Merge Cleaned Data for Stokesy **#
#************************************#

t3a    = merge(train2dep[, !names(train2dep) == "OutcomeType"], train2type[,  !names(train2type) == "AnimalType"],                                    by = "AnimalID")
t3b    = merge(t3a,                                             train2name[,  !names(train2name) == "Name"],                                          by = "AnimalID")
t3c    = merge(t3b,                                             train2age[,   !names(train2age)  == "AgeuponOutcome"],                                by = "AnimalID")
t3d    = merge(t3c,                                             train2sex[,   !names(train2sex)  == "SexuponOutcome"],                                by = "AnimalID")
t3e    = merge(t3d,                                             train2date[,  !names(train2date) == "DateTime"],                                      by = "AnimalID")
t3f    = merge(t3e,                                             train2breed[, !names(train2breed) %in% c("Breed", "mix", "parenta", "parentb")],      by = "AnimalID") 
train3 = merge(t3f,                                             train2color[, !names(train2color) %in% c("AnimalType", "Color", "color1", "color2")], by = "AnimalID")

rm(t3a, t3b, t3c, t3d, t3e, t3f)









#**************************#
#** Fill in Missing Data **#
#**************************#



#** Playing with Distributions **#


temp = train3[which(is.na(train3$age) == TRUE),]
temp = train3[which(is.na(train3$gender) == TRUE),]
temp = train3[which(is.na(train3$intact) == TRUE),]


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
