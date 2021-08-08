library(dslabs)
library(dplyr)
library(caret)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Cemprehension Check Part 1
table(x)
table(x,y)
prop.table(table(data$type,dat$sex))
prop.table(table(x))
#Q1 answer= dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))
#guess based on type (inclass or online)
z<-ifelse(dat$type=="inclass", "Female", "Male")%>%factor()
confusionMatrix(data= z, reference= dat$sex)
confusionMatrix(data = z, reference =factor(dat$sex, c("Female", "Male")) )
table(ifelse(dat$type=="inclass", "Female", "Male")%>%factor(), factor(dat$sex, c("Female", "Male")))

#Comprehension Check #2
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
# line of 
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
min(train$Sepal.Length) 
max(train$Sepal.Length)
sep_Len_cutoff<-seq(5,7.9,.1)
ac_sep_len<-map_dbl(sep_Len_cutoff, function(x){
  species_hat<-ifelse(train$Sepal.Length>x, "versicolor", "virginica")%>%factor(levels=levels(train$Species))
  mean(species_hat==train$Species)
})
max(ac_sep_len)
max_ac_sep_len<-ifelse(train$Sepal.Length>.5, "versicolor", "virginica")%>%factor()
confusionMatrix(data=max_ac_sep_len, reference=factor(train$Species, c("versicolor", "virginica")))

#test Git
                