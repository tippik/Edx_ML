library(dslabs)
library(dplyr)
library(caret)
library(purrr)
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
train<-iris[-test_index,]
min(train$Petal.Width) 
max(train$Petal.Width)
pet_wid_cutoff<-seq(min(train$Petal.Width),max(train$Petal.Width),.1)
ac_pet_wid<-map_dbl(pet_wid_cutoff, function(x){
  species_hat<-ifelse(train$Petal.Width>x, "versicolor", "virginica")%>%
    factor(levels = levels(y))
  mean(species_hat==train$Species)
})
data.frame(pet_wid_cutoff, ac_pet_wid) %>% 
  ggplot(aes(pet_wid_cutoff, ac_pet_wid)) + 
  geom_point() + 
  geom_line() 
best_pet_wid_cutoff<-pet_wid_cutoff[which.max(ac_pet_wid)]
test_pet_wid<-map_dbl(pet_wid_cutoff, function(x){
  species_hat<-ifelse(test$Petal.Width>best_pet_wid_cutoff, "versicolor", "virginica")%>%
    factor(levels = levels(y))
  mean(species_hat==test$Species)
})
(max(test_sep_wid))

confusionMatrix(data=max_ac_sep_len, reference=train$Species)


                