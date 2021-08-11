mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean


# Comprehension Check: Practice with Machine Learning, Part 2  
#Q8
min(train$Sepal.Width) 
max(train$Sepal.Width)
sep_wid_cutoff<-seq(min(train$Sepal.Width),max(train$Sepal.Width),.1)
ac_sep_wid<-map_dbl(sep_wid_cutoff, function(x){
  species_hat<-ifelse(train$Sepal.Width>x, "versicolor", "virginica")%>%
    factor(levels = levels(y))
  mean(species_hat==train$Species)
})
data.frame(sep_wid_cutoff, ac_sep_wid) %>% 
  ggplot(aes(sep_wid_cutoff, ac_sep_wid)) + 
  geom_point() + 
  geom_line() 

best_sep_wid_cutoff<-sep_wid_cutoff[which.max(ac_sep_wid)]
(max(ac_sep_len))
max_ac_sep_len<-ifelse(test$Sepal.Length>best_sep_wid_cutoff, "versicolor", "virginica")%>%
  factor(levels=levels(y))
confusionMatrix(data=max_ac_sep_len, reference=test$Species)
#Q8 