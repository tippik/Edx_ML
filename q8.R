#Answer for Q8


foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	


#Answer q9

predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

#Answer Q10
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	

plot(iris,pch=21,bg=iris$Species)


test<-ifelse((test$Petal.Length>4.7|| test$Petal.Width>1.5), "virginica","versicolor")
mean(test==test$Species)








# Comprehension Check: Practice with Machine Learning, Part 2  
#Q8
min(train$Sepal.Width) 
max(train$Sepal.Width)
sep_wid_cutoff<-seq(min(train$Sepal.Width),max(train$Sepal.Width),.1)
ac_sep_wid<-map_dbl(sep_wid_cutoff, function(x){
  species_hat<-ifelse(train$Sepal.Width>x, "virginica", "versicolor")%>%
    factor(levels = levels(y))
  mean(species_hat==train$Species)
})
data.frame(sep_wid_cutoff, ac_sep_wid) %>% 
  ggplot(aes(sep_wid_cutoff, ac_sep_wid)) + 
  geom_point() + 
  geom_line() 

best_sep_wid_cutoff<-sep_wid_cutoff[which.max(ac_sep_wid)]
(max(ac_sep_len))
test_sep_wid<-ifelse(test$Sepal.Width>2.4, "versicolor", "virginica")%>%
    factor(levels = levels(y))
mean(test_sep_wid==test$Species)

#(max(test_sep_wid))
#max_ac_sep_len<-ifelse(test$Sepal.Length>best_sep_wid_cutoff, "versicolor", "virginica")%>%
  factor(levels=levels(y))
#confusionMatrix(data=max_ac_sep_len, reference=test$Species)
#Q8 Petal.Width 
min(train$Petal.Width) 
max(train$Petal.Width)
pet_wid_cutoff<-seq(min(train$Petal.Width),max(train$Petal.Width),.1)
ac_pet_wid<-map_dbl(pet_wid_cutoff, function(x){
  species_hat<-ifelse(train$Petal.Width>x, "virginica", "versicolor")%>%
    factor(levels = levels(y))
  mean(species_hat==train$Species)
})
data.frame(pet_wid_cutoff, ac_pet_wid) %>% 
  ggplot(aes(pet_wid_cutoff, ac_pet_wid)) + 
  geom_point() + 
  geom_line() 
best_pet_wid_cutoff<-pet_wid_cutoff[which.max(ac_pet_wid)]
test_pet_wid<-ifelse(test$Petal.Width>1.5, "virginica", "versicolor")
mean(test_pet_wid==test$Species)



min(train$Petal.Length) 
max(train$Petal.Length)
pet_L_cutoff<-seq(min(train$Petal.Length),max(train$Petal.Length),.1)
ac_pet_L<-map_dbl(pet_L_cutoff, function(x){
  species_hat<-ifelse(train$Petal.Length>x, "virginica","versicolor")%>%
    factor(levels = levels(y))
  mean(species_hat==train$Species)
})
data.frame(pet_L_cutoff, ac_pet_L) %>% 
  ggplot(aes(pet_L_cutoff, ac_pet_L)) + 
  geom_point() + 
  geom_line() 
best_pet_L_cutoff<-pet_L_cutoff[which.max(ac_pet_L)]
test_pet_L<-ifelse(test$Petal.Length>best_pet_L_cutoff, "virginica", "versicolor")
  
mean(test_pet_L==test$Species)





min(train$Sepal.Length) 
max(train$Sepal.Length)
pet_sepL_cutoff<-seq(min(train$Sepal.Length),max(train$Sepal.Length),.1)
ac_sep_L<-map_dbl(pet_sepL_cutoff, function(x){
  species_hat<-ifelse(train$Sepal.Length>x, "virginica", "versicolor" )%>%
    factor(levels = levels(y))
  mean(species_hat==train$Species)
})

data.frame(pet_sepL_cutoff, ac_sep_L) %>% 
  ggplot(aes(pet_sepL_cutoff, ac_sep_L)) + 
  geom_point() + 
  geom_line() 
best_sepL_cutoff<-pet_sepL_cutoff[which.max(ac_sep_L)]
test_sepL<-ifelse(test$Sepal.Length>best_sepL_cutoff, "virginica","versicolor")%>%
    factor(levels = levels(y))
  
mean(test_sepL==test$Species)

