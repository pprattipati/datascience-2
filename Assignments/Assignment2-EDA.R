# install.packages("dplyr")
# install.packages("tm")
library(ggplot2)
library(dplyr)

# Problem1: Exploring Religious Terror Attacks
# Go through the following problem of kaggle:
#   https://www.kaggle.com/argolof/predicting-terrorism
# Do the following tasks:
# a. Download the dataset from the following link:
# b. Load the dataset and do the required type conversions
setwd("D:/Data Science/Algorithmica/Assignments/PredictingTerrorism")
attacks_train = read.csv("attacks_data_UTF8.csv", header=TRUE, stringsAsFactors = FALSE)
dim(attacks_train)
str(attacks_train)
# attacks_train$Country = as.factor(attacks_train$Country)

# c. Explore all the attributes individually using univariate numeric and graphics
summary(attacks_train)
xtabs(~Country, attacks_train)
X11()
ggplot(attacks_train) + geom_bar(aes(x=Country))

# d. Do the following:
# Need the 'dplyr' package to do this efficiently
select(attacks_train, Country) # Show Country
head(select(attacks_train, -Description)) # Show everything other than Description
select(attacks_train, -Description) %>% arrange(Country) %>% filter(Killed > 1) %>% head

#     a. Find top-10 countries with most attacks, most injured and most killed respectively and show them with plots

# top-10 countries with most attacks
top_country_attacks = attacks_train %>%  group_by(Country) %>% summarise(totalAttacks=n()) %>% arrange(desc(totalAttacks)) %>% head(n=10)
top_country_attacks
X11()
ggplot(top_country_attacks) + geom_bar(aes(x=Country, y=totalAttacks), stat="identity") + coord_flip()

# top-10 countries with most injured
top_country_injured = attacks_train %>%  group_by(Country) %>% summarise(totalInjured=sum(Injured)) %>% arrange(desc(totalInjured)) %>% head(n=10)
top_country_injured
X11()
ggplot(top_country_injured) + geom_bar(aes(x=Country, y=totalInjured), stat="identity") + coord_flip()

# top-10 countries with most killed
top_country_killed = attacks_train %>%  group_by(Country) %>% summarise(totalKilled=sum(Killed)) %>% arrange(desc(totalKilled)) %>% head(n=10)
top_country_killed
X11()
ggplot(top_country_killed) + geom_bar(aes(x=Country, y=totalKilled), stat="identity") + coord_flip()

# The same stats as above were computed the 'hard way' without using the 'dplyr' package
# with most attacks
# country_count = data.frame(table(attacks_train$Country))
# country_attacks_order = order(country_count$Freq, decreasing = TRUE)
# top_country_attacks = country_count[head(country_attacks_order, n=10), ]
# names(top_country_attacks) = c("Countries", "Attacks")
# X11()
# ggplot(top_country_attacks) + geom_bar(aes(x=Countries, y=Attacks), stat="identity")
# 
# # gathering data for total killed and injured in each country
# Countries = sort(unique(attacks_train$Country))
# country_count = length(Countries)
# Injured = numeric(length = country_count)
# Killed = numeric(length = country_count)
# Attacked = numeric(length = country_count)
# for (i in 1:country_count)
# {
#   Injured[i] = sum(attacks_train[attacks_train$Country == Countries[i], "Injured"])
#   Killed[i] = sum(attacks_train[attacks_train$Country == Countries[i], "Killed"])
#   Attacked[i] = nrow(attacks_train[attacks_train$Country == Countries[i], ])
# }
# 
# country_stats = data.frame(Countries, Injured, Killed, Attacked)
# 
# # Top 10 countries most injured
# country_injured_order = order(country_stats$Injured, decreasing = TRUE)
# top_country_injured = country_stats[head(country_injured_order, n=10), c("Countries", "Injured")]
# top_country_injured
# ggplot(top_country_injured) + geom_bar(aes(x=Countries, y=Injured), stat="identity")
# 
# # Top 10 countries most killed
# country_killed_order = order(country_stats$Killed, decreasing = TRUE)
# top_country_killed = country_stats[head(country_killed_order, n=10), c("Countries", "Killed")]
# top_country_killed
# ggplot(top_country_killed) + geom_bar(aes(x=Countries, y=Killed), stat="identity")


#     b. Find top-10 cities with most attacks, most injured and most killed respectively and show them with plots

# We might first want to clean up mis-spellings of cities. But this is a very laborious task
# e.g. "Mogadishu", "Mogadisu", "Mogadisuh", "Mogadushu".
attacks_train$City[attacks_train$City %in% c('Mogadisu', 'Mogadisuh', 'Mogadushu')] = 'Mogadishu'

# top-10 cities with most attacks
top_city_attacks = attacks_train %>%  group_by(City) %>% summarise(totalAttacks=n()) %>% arrange(desc(totalAttacks)) %>% head(n=10)
top_city_attacks
X11()
ggplot(top_city_attacks) + geom_bar(aes(x=City, y=totalAttacks), stat="identity") + coord_flip()

# top-10 cities with most injured
top_city_injured = attacks_train %>%  group_by(City) %>% summarise(totalInjured=sum(Injured)) %>% arrange(desc(totalInjured)) %>% head(n=10)
top_city_injured
X11()
ggplot(top_city_injured) + geom_bar(aes(x=City, y=totalInjured), stat="identity") + coord_flip()

# top-10 cities with most killed
top_city_killed = attacks_train %>%  group_by(City) %>% summarise(totalKilled=sum(Killed)) %>% arrange(desc(totalKilled)) %>% head(n=10)
top_city_killed
X11()
ggplot(top_city_killed) + geom_bar(aes(x=City, y=totalKilled), stat="identity") + coord_flip()

# The same stats as above were computed the 'hard way' without using the 'dplyr' package
# gathering data for total killed and injured in each city
# Cities = sort(unique(attacks_train$City))
# city_count = length(Cities)
# Injured = numeric(length = city_count)
# Killed = numeric(length = city_count)
# Attacked = numeric(length = city_count)
# for (i in 1:city_count)
# {
#   Injured[i] = sum(attacks_train[attacks_train$City == Cities[i], "Injured"])
#   Killed[i] = sum(attacks_train[attacks_train$City == Cities[i], "Killed"])
#   Attacked[i] = nrow(attacks_train[attacks_train$City == Cities[i], ])
# }
# 
# city_stats = data.frame(Cities, Injured, Killed, Attacked)
# 
# # Top 10 cities most attacked
# city_attacked_order = order(city_stats$Attacked, decreasing = TRUE)
# top_city_attacked = city_stats[head(city_attacked_order, n=10), c("Cities", "Attacked")]
# top_city_attacked
# X11()
# ggplot(top_city_attacked) + geom_bar(aes(x=Cities, y=Attacked), stat="identity")
# 
# # Top 10 cities most injured
# city_injured_order = order(city_stats$Injured, decreasing = TRUE)
# top_city_injured = city_stats[head(city_injured_order, n=10), c("Cities", "Injured")]
# top_city_injured
# ggplot(top_city_injured) + geom_bar(aes(x=Cities, y=Injured), stat="identity")
# 
# # Top 10 cities most killed
# city_killed_order = order(city_stats$Killed, decreasing = TRUE)
# top_city_killed = city_stats[head(city_killed_order, n=10), c("Cities", "Killed")]
# top_city_killed
# ggplot(top_city_killed) + geom_bar(aes(x=Cities, y=Killed), stat="identity")

#     c. Draw a plot that shows the relationship between killed and top-4 countries
# extracting data for the top 4 countries with most killed
top4Killed_attacks_train = attacks_train[attacks_train$Country %in% top_country_killed$Country[1:4],]
X11()
# This plot gives the density of various 'killed' values grouped by country. This is still not clear enough.
ggplot(top4Killed_attacks_train) + geom_density(aes(x=Killed, group=Country, color=Country))
# This plot is much clearer with log().
# See the log curve. For small values of x, it makes them more apart; but for large values of x, it makes them 
# 'closer'. Use log() to spread out the data, when a lot of data is present in a small range and less data is
# present in larger range.
ggplot(top4Killed_attacks_train) + geom_density(aes(x=log2(Killed), group=Country, color=Country))

# e. Clean the description column using following steps:
#     a. Normalize text: convert entire text to lower case
library(tm)
# install.packages("SnowballC")
# install.packages("wordcloud")
library(SnowballC)
library(wordcloud)
ls("package:tm")
dim(attacks_train)
attacks_train$Description = tolower(attacks_train$Description)
#     b. Remove numbers
attacks_train$Description[9017:9020]
attacks_train$Description = removeNumbers(attacks_train$Description)
#     c. Remove whitespaces
attacks_train$Description = stripWhitespace(attacks_train$Description)
#     d. Remove punctuation symbols
attacks_train$Description[9992:9997]
attacks_train$Description = removePunctuation(attacks_train$Description)
#     e. Remove stop words
attacks_train$Description[9995:9997]
attacks_train$Description = removeWords(attacks_train$Description, stopwords(kind = "en"))
# ASK: Understandable why the below ones are needed, but look ugly. Can we avoid these?
# Do this again to remove the whitespaces introduced by removeWords()
attacks_train$Description = stripWhitespace(attacks_train$Description)
# This is to remove the whitespaces at the beginning of string; they are not removed by stripWhiteSpace()
library(stringr)
attacks_train$Description = str_trim(attacks_train$Description)

#     f. Stem the words # liking->like, use the text mining 'tm' package, and the 'stem()' function
# attacks_train$Description[9992:9997]
# wordStem(c("killing", "killed", "kill"), language = "en")
# strsplit(attacks_train$Description[9992:9997], split="\\s")

# ASK: Stemming all words together instead of stemming each description. Is that OK?
words_all = unlist(strsplit(attacks_train$Description, split="\\s"))
stem_words = wordStem(words_all, language = "en")

# f. Find out the most frequent words across all the attacks from the cleaned description column

# ASK: Could do only with 'dplyr' package. Got memory issues solving it with 'tm' package. Is that OK?

# corp = Corpus(VectorSource(stem_words)) # takes a long time
# tdm = TermDocumentMatrix(corp) # takes a long time
# findFreqTerms(tdm,3500) # not useful to find the most frequent words and their frequencies
# 
# # Running out of memory while doing this - cannot allocate vector of size 8.4 Gb
# # Cannot use this approach
# m = as.matrix(tdm)
# v = sort(rowSums(m), decreasing = TRUE)
# head(v, 10)
# 
# # Trying to overcome the memory issue
# temp = inspect(tdm) # Getting memory error again
# stem_words_freq = data.frame(stem_words = rownames(temp), freq = rowSums(temp))

# Trying with dplyr package
stem.df = data.frame(stem_words)
stem.df$stem_words = as.character(stem.df$stem_words)
# Get the top most frequent word stems
stem_words_freq = stem.df %>%  group_by(stem_words) %>% summarise(freq=n()) %>% arrange(desc(freq))

# g. Draw the wordcloud showing the frequencies of the words used across attacks and also show them via barplot
# ASK: Cannot show all words. How to decide the cut-off?
# Showing the top 100 most frequent in wordcloud, and 30 in barplot
X11()
wordcloud(stem_words_freq$stem_words[1:100], stem_words_freq$freq[1:100])
ggplot(head(stem_words_freq, 30)) + geom_bar(aes(x=stem_words, y=freq), stat="identity") + coord_flip()

# h. Find the correlations among each pair of words and cluster the words based on correlation
# ASK: How to do this???



# Problem 2: Explore Car dataset
# ==============================
# Download the car.data from following link:
#   https://github.com/algorithmica-repository/datascience/tree/master/datasets
# Here are the descriptions of the attributes of the car dataset:
#   buying: vhigh, high, med, low.
#   maint: vhigh, high, med, low.
#   doors: 2, 3, 4, 5more.
#   persons: 2, 4, more.
#   lug_boot: small, med, big.
#   safety: low, med, high.
# The output 'class' attribute can take one of the following values:
#   unacc, acc, good, vgood
# Do the following tasks:
#   a. Load the dataset into frame and convert all the attributes to factor type.
setwd("D:/Data Science/Algorithmica/Assignments/Car")
car_train = read.csv("car.data", header=FALSE, stringsAsFactors = TRUE)
names(car_train) = c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")
str(car_train)

#   b. Explore all the attributes individually using univariate numerics and graphics.
library(ggplot2)
xtabs(~class, car_train) # 70% - unacc, 22% - acc, 4% - good, 4% - vgood
ggplot(car_train) + geom_bar(aes(x=class))
# all the below attributes have uniformly distributed values
xtabs(~buying, car_train) # each of the 4 values occur 25% of the time
xtabs(~maint, car_train) # each of the 4 values occur 25% of the time
xtabs(~doors, car_train) # each of the 4 values occur 25% of the time
xtabs(~persons, car_train) # each of the 3 values occur 1/3rd of the time
xtabs(~lug_boot, car_train) # each of the 3 values occur 1/3rd of the time
xtabs(~safety, car_train) # each of the 3 values occur 1/3rd of the time

#   c. Explore all the bivariate relationships numerically and graphically.
xtabs(~buying + class, car_train)
# high & vhigh buying => either 'unacc' or 'acc'; never 'good' or 'vgood'
# low buying are the most good/vgood. So, cheaper cars are generally preferred more. 
ggplot(car_train) + geom_bar(aes(x=buying, fill=class))
# vhigh maintenance never 'good' or 'vgood'. Low maintenace cars generally preferred
ggplot(car_train) + geom_bar(aes(x=maint, fill=class))
# number of 'doors' has very little impact on car preference
ggplot(car_train) + geom_bar(aes(x=doors, fill=class))
# seating capacity of 2 always 'unacc'. Not much difference between '4' and 'more'
ggplot(car_train) + geom_bar(aes(x=persons, fill=class))
# small lug_boot never 'vgood'. Otherwise, don't make much difference
ggplot(car_train) + geom_bar(aes(x=lug_boot, fill=class))
# low safety always 'unacc'. Only high safety is 'vgood'
ggplot(car_train) + geom_bar(aes(x=safety, fill=class))

#   d. What features do you recommend for predicting class category and why?
# Safety, seating capacity, buying & maintenace. Cars with low safety and seating capacity of just 2
# are always 'unacc'. high/vhigh buying and vhigh maintenance are always 'unacc' or 'acc' at best.

#   e. What kind of patterns have you discovered with the above explorations?
#     I have described the patterns inline while exploring bi-variate relationships.
# Exploring a tri-variate relationship for fun.
# If buying/maintenace are both vhigh or have high/vhigh combination, then 'unacc'
X11()
ggplot(car_train) + geom_bar(aes(x=buying, fill=class)) + facet_grid(maint~.)


# Problem 3: Exploring Kidney data
# Download the chronic_kidney_data.txt from following link:
#   https://github.com/algorithmica-repository/datascience/tree/master/datasets/
#   The description of the dataset can be found at following link:
#   http://archive.ics.uci.edu/ml/datasets/Chronic_Kidney_Disease
# Do the following tasks:
#   a. Load the dataset into frame and convert all the attributes to factor type.
setwd("D:/Data Science/Algorithmica/Assignments/Kidney")
# ASK: Are we supposed to clean these manually in the file?
# Removed an extra ',' from line 370. Removed '\t' from 13 rows; cleaned using 'Notepad++'
# Attribute 'dm' had a ' yes' which I corrected in the file to 'yes'
kidney_train = read.csv("chronic_kidney_data.txt", header=FALSE, stringsAsFactors = TRUE, sep=c(",", "\\t"), na.strings = c("?"))
names(kidney_train) = c("age","bp","sg","al","su","rbc","pc","pcc","ba","bgr","bu","sc","sod","pot","hemo","pcv","wbcc","rbcc","htn","dm","cad","appet","pe","ane","class")
kidney_train$sg = factor(kidney_train$sg)
kidney_train$al = factor(kidney_train$al)
kidney_train$su = factor(kidney_train$su)
str(kidney_train)
dim(kidney_train)

# attribute 'age' numeric
# attribute 'bp'  numeric
# attribute 'sg' {1.005,1.010,1.015,1.020,1.025}
# attribute 'al' {0,1,2,3,4,5}  
# attribute 'su' {0,1,2,3,4,5}  
# attribute 'rbc' {normal,abnormal}
# attribute 'pc' {normal,abnormal} 
# attribute 'pcc' {present,notpresent}
# attribute 'ba' {present,notpresent}
# attribute 'bgr'  numeric
# attribute 'bu' numeric
# attribute 'sc' numeric
# attribute 'sod' numeric
# attribute 'pot' numeric
# attribute 'hemo' numeric
# attribute 'pcv' numeric
# attribute 'wbcc' numeric
# attribute 'rbcc' numeric
# attribute 'htn' {yes,no}
# attribute 'dm' {yes,no}
# attribute 'appet' {good,poor}
# attribute 'cad' {yes,no}
# attribute 'pe' {yes,no} 
# attribute 'ane' {yes,no}
# attribute 'class' {ckd,notckd}

#   b. Explore all the attributes individually using univariate numerics and graphics.
summary(kidney_train)
# numerics and graphics for a 'categorical' attribute - bar chart
xtabs(~pc, kidney_train)
X11()
ggplot(kidney_train) + geom_bar(aes(x=pc))
# numerics and graphics for a 'continuous' attribute - histogram and boxplot
xtabs(~bgr, kidney_train)
ggplot(kidney_train) + geom_histogram(aes(x=bgr))
ggplot(kidney_train) + geom_boxplot(aes(x=factor(0), y=bgr))

#   c. Explore all the bivariate relationships numerically and graphically.
# bivariate details for a 'categorical' attribute
xtabs(~su + class, kidney_train) # if sugar > 0, always disease ('ckd'). If sugar == 0, 50% chance of disease
ggplot(kidney_train) + geom_bar(aes(x=su, fill=class))

# bivariate details for a 'continuous' attribute
xtabs(~age + class, kidney_train) # age does not give much indication of disease
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=age)) + coord_flip() # median age of diseased = 60, non-diseased = 45
ggplot(kidney_train) + geom_histogram(aes(x=age)) + facet_grid(class ~ .)

xtabs(~bp + class, kidney_train) # bp >= 90 => diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=bp)) + coord_flip() # median bp of diseased = 80, non-diseased = 60
ggplot(kidney_train) + geom_histogram(aes(x=bp)) + facet_grid(class ~ .)

xtabs(~sg + class, kidney_train) # sg < 1.02 => always disease
ggplot(kidney_train) + geom_bar(aes(x=sg, fill=class)) # As sg increases, chances of disease decreases

xtabs(~al + class, kidney_train) # al > 0 => always disease; (al == 0) => 25% chance of disease
ggplot(kidney_train) + geom_bar(aes(x=al, fill=class))

xtabs(~rbc + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=rbc, fill=class)) # abnormal rbc => always disease; normal => 1/3rd chance of disease

xtabs(~pc + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=pc, fill=class)) # abnormal pc => always disease; normal => 50% chance of disease

xtabs(~pcc + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=pcc, fill=class)) # pcc present => always disease; not present => 65% chance of disease

xtabs(~ba + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=ba, fill=class)) # ba present => always disease; not present => 65% chance of disease

xtabs(~htn + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=htn, fill=class)) # htn is yes => always disease; no => 40% chance of disease

xtabs(~dm + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=dm, fill=class)) # dm is yes => always disease; no => 40% chance of disease

xtabs(~cad + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=cad, fill=class)) # cad is yes => always disease; not present => 65% chance of disease

xtabs(~appet + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=appet, fill=class)) # appet is poor => always disease; good => 50% chance of disease

xtabs(~pe + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=pe, fill=class)) # pe is yes => always disease; no => 50% chance of disease

xtabs(~ane + class, kidney_train)
ggplot(kidney_train) + geom_bar(aes(x=ane, fill=class)) # ane is yes => always disease; no => 50% chance of disease

xtabs(~bgr + class, kidney_train) # bgr > 140 => diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=bgr)) + coord_flip() # median bp of diseased ~ 145, non-diseased ~ 110
ggplot(kidney_train) + geom_histogram(aes(x=bgr)) + facet_grid(class ~ .)

xtabs(~bu + class, kidney_train)  # bu > 50 => diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=bu)) + coord_flip()
ggplot(kidney_train) + geom_histogram(aes(x=bu)) + facet_grid(class ~ .) # bu > ?? => diseased

xtabs(~sc + class, kidney_train)  # sc > 1.2 => diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=sc)) + coord_flip()
ggplot(kidney_train) + geom_histogram(aes(x=sc)) + facet_grid(class ~ .)

xtabs(~sod + class, kidney_train)  # sod < 135 => diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=sod)) + coord_flip()
ggplot(kidney_train) + geom_histogram(aes(x=sod)) + facet_grid(class ~ .)

xtabs(~pot + class, kidney_train)  # pot > 5.0 => diseased, < 3.3 => not diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=pot)) + coord_flip()
ggplot(kidney_train) + geom_histogram(aes(x=pot)) + facet_grid(class ~ .)

xtabs(~hemo + class, kidney_train)  # hemo < 13 => diseased, > 16.1 => not diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=hemo)) + coord_flip()
ggplot(kidney_train) + geom_histogram(aes(x=hemo)) + facet_grid(class ~ .)

xtabs(~pcv + class, kidney_train)  # pcv < 40 => diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=pcv)) + coord_flip()
ggplot(kidney_train) + geom_histogram(aes(x=pcv)) + facet_grid(class ~ .)

xtabs(~wbcc + class, kidney_train)  # wbcc >= 11200 => diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=wbcc)) + coord_flip()
ggplot(kidney_train) + geom_histogram(aes(x=wbcc)) + facet_grid(class ~ .)

xtabs(~rbcc + class, kidney_train)  # rbcc < 4.4 => diseased
ggplot(kidney_train) + geom_boxplot(aes(x=class, y=rbcc)) + coord_flip()
ggplot(kidney_train) + geom_histogram(aes(x=rbcc)) + facet_grid(class ~ .)

#   d. What features do you recommend for predicting the disease is chronic or not and why?
# Apart from age, all other features are good predictors of the disease. In all of them, there are a range/set of values
# which indicate that the disease is chronic. 'dm' and 'htn' are particularly good because a large fraction of patients are
# 'yes' and they are all chronic. 'rbc' has a lot of NAs and hence not so reliable.

#   e. What kind of patterns have you discovered with the above explorations?
# Discussed inline in the comments section during bi-variate analysis.