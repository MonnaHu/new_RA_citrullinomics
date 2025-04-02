setwd("H:/20250330 figure+code/Figure 5/Predicting MTX+LEF response & Fig. 5b-h")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(venn)
library(caret)    
library(smotefamily)    
library(ROSE)      
library(glmnet) 
library(randomForest)
library(xgboost)  
library(lightgbm)   
library(nnet)      
library(e1071)    
library(pROC)
library(scales)


rm(list=ls())

#################################################################### 5-fold feature selection
##################################### input data
d1 = read.csv("H:/20250330 figure+code/Figure 5/ML dataset 1 for machine learning.csv",row.names = 1)
table(d1$csDMARDs)
df = d1[,-c(1,3,4)]; colnames(df)[1]="target"
names(d1)
yes = df %>% filter(target==1)
no = df %>% filter(target==0)


####################################  input external data
d2 = read.csv("H:/20250330 figure+code/Figure 5/ML dataset 2 for machine learning.csv",row.names = 1)
colnames(d2)[2]="target"


#################################### 5-fold
seed=c(1,12,123,1234,12345)

cv5_sample <- function(data,n){
  lapply(c(seed), function(seed){
    result=NULL
    for(i in seed){
      set.seed(i)
      data = data[!data%in%result$re]
      if(length(data) < round(n/5)+2){re = data}else{re = sample(data, round(n/5))}
      re = data.frame(re,seed=i)
      result = rbind(result, re)
    }
    return(result)
  })
}

sample_yes = do.call(rbind, cv5_sample(rownames(yes), nrow(yes)))
sample_no = do.call(rbind,cv5_sample(rownames(no), nrow(no)))

sample = lapply(seed, function(s){data = c(sample_yes$re[sample_yes$seed==s], sample_no$re[sample_no$seed==s])})
names(sample) = 1:5


################################# classifiers
models_list <- list(
  "1Logistic Regression" = "glm",
  "2ridge"="glmnet",
  "3lasso"="glmnet",
  "4KNN" = "knn",
  "5SVM" = "svmRadial",
  "6Decision Tree" = "rpart",
  "7Random Forest" = "rf",
  "8XGBoost" = "xgbTree",
  "9LightGBM" = "gbm"
)


################################################################
Features = NULL
i=1

for (i in 1:5) {
  training = df[!(rownames(df) %in% sample[[i]]),] # k-1 folds
  
  ################################ oversampleing & SMOTE
  t_yes = training[training$target==1,]
  set.seed(123); t_SMOTE <-  SMOTE(training[,2:ncol(training)],training[1],dup_size = 3, K=5)$data 
  data1 = t_SMOTE[,c(ncol(t_SMOTE), 1:(ncol(t_SMOTE)-1))]; colnames(data1)[1] ="target"; table(data1$target)
  data1 = data1 %>% filter(target==1)
  training2 = rbind(training,t_yes,t_yes[sample(1:nrow(t_yes),15),])
  set.seed(123); data = rbind(training2, data1[sample(1:nrow(data1),table(training2$target)["0"]-table(training2$target)["1"]),]); table(data$target)
  
  # 打乱顺序
  set.seed(123); data = data[sample(1:nrow(data), nrow(data)),]
  
  x = as.matrix(data[,2:ncol(data)])
  y = as.numeric(data$target)
  
  # feature selection
  # ① LASSO 
  set.seed(123); fitcv<-cv.glmnet(x,y,family = "binomial",alpha = 1,type.measure = "auc", nfolds=5)
  set.seed(123); lasso_model <- glmnet(x, y, alpha=1, family="binomial", lambda=fitcv$lambda.min)
  sf_lasso <- colnames(x)[which(coef(lasso_model) != 0)-1]  
  
  # ② RF 
  set.seed(123);ctrl <- trainControl(method = "cv", number = 5)
  set.seed(123);rf_model <- train(x = x, y = as.factor(y), method = "rf",trControl = ctrl,tuneGrid = expand.grid(mtry = seq(1,ncol(x),2)))
  rf <-rf_model$finalModel 
  importance = importance(rf) %>% data.frame() %>% arrange(-MeanDecreaseGini)
  # barplot(importance[order(importance$MeanDecreaseGini),],horiz = T,beside = T) # TOP15
  sf_rf <- rownames(importance)[1:15]
  
  # ③ REF+RF
  set.seed(123); control <- rfeControl(functions = rfFuncs,method = "cv", number = 5)
  set.seed(123); rfe_results <- rfe(x = x,y = y,sizes = seq(1, ncol(x), by = 3), rfeControl = control, method = "rf")
  sf_rfrfe <- predictors(rfe_results)
  
  # ④ REF+SVM
  set.seed(123); control <- rfeControl(functions = rfFuncs,method = "cv", number = 5)
  set.seed(123); rfe_results <- rfe(x = x,y = y,sizes = seq(1, ncol(x), by = 3), rfeControl = control, method = "svmLinear")
  sf_svmrfe <-  predictors(rfe_results)
  
  sf <- list(Lasso=sf_lasso,RF=sf_rf,RFERF=sf_rfrfe,RFESVM=sf_svmrfe)
  
  l=3
  for(l in 1:length(sf)){
    features <- sf[[l]]
    Features = rbind(Features, data.frame(features=sf[[l]]) %>% mutate(Fold=i, f = names(sf)[l]))
  }
}

#  overlap
freq = Features
names(freq)
freq2 = reshape2::dcast(freq, features ~ f, value.var = "Fold") %>%
  arrange(desc(Lasso),desc(RF),desc(RFERF),desc(RFESVM))
features <- freq2$features[freq2$Lasso>0 & freq2$RF>0 & freq2$RFERF>0 & freq2$RFESVM>0]
features

write.csv(Features,"5-fold 4_features_selection results.csv", row.names = F)



###################################################### selecting classifier
rm(list=ls())
freq = read.csv("5-fold 4_features_selection results.csv")
names(freq)
freq2 = reshape2::dcast(freq, features ~ f, value.var = "Fold") %>%
  arrange(desc(Lasso),desc(RF),desc(RFERF),desc(RFESVM))
features <- freq2$features[freq2$Lasso>0 & freq2$RF>0 & freq2$RFERF>0 & freq2$RFESVM>0]
features

d2 = read.csv("H:/20250330 figure+code/Figure 5/ML dataset 2 for machine learning.csv",row.names = 1)
colnames(d2)[2]="target"


d1 = read.csv("H:/20250330 figure+code/Figure 5/ML dataset 1 for machine learning.csv",row.names = 1)
table(d1$csDMARDs)
df = d1[,-c(1,3,4)]; colnames(df)[1]="target"
names(d1)
yes = df %>% filter(target==1)
no = df %>% filter(target==0)


#################################### 5-fold
set.seed(1);seed = sample(1:10000,5)

cv5_sample <- function(data,n){
  lapply(c(seed), function(seed){
    result=NULL
    for(i in seed){
      set.seed(i)
      data = data[!data%in%result$re]
      if(length(data) < round(n/5)+2){re = data}else{re = sample(data, round(n/5))}
      re = data.frame(re,seed=i)
      result = rbind(result, re)
    }
    return(result)
  })
}

sample_yes = do.call(rbind, cv5_sample(rownames(yes), nrow(yes)))
sample_no = do.call(rbind,cv5_sample(rownames(no), nrow(no)))

sample = lapply(seed, function(s){data = c(sample_yes$re[sample_yes$seed==s], sample_no$re[sample_no$seed==s])})
names(sample) = 1:5

models_list <- list(
  "1Logistic Regression" = "glm",
  "2ridge"="glmnet",
  "3lasso"="glmnet",
  "4KNN" = "knn",
  "5SVM" = "svmRadial",
  "6Decision Tree" = "rpart",
  "7Random Forest" = "rf",
  "8XGBoost" = "xgbTree",
  "9LightGBM" = "gbm"
)

results=NULL
i=1
for (i in 1:5) {
  validation = df[rownames(df) %in% sample[[i]],]  
  training = df[!(rownames(df) %in% sample[[i]]),] 
  
  ################################ 
  t_yes = training[training$target==1,]
  set.seed(123); t_SMOTE <-  SMOTE(training[,2:ncol(training)],training[1],dup_size = 3, K=5)$data 
  data1 = t_SMOTE[,c(ncol(t_SMOTE), 1:(ncol(t_SMOTE)-1))]; colnames(data1)[1] ="target"; table(data1$target)
  data1 = data1 %>% filter(target==1)
  training2 = rbind(training,t_yes,t_yes[sample(1:nrow(t_yes),15),])
  set.seed(123); data = rbind(training2, data1[sample(1:nrow(data1),table(training2$target)["0"]-table(training2$target)["1"]),]); table(data$target)
  set.seed(123); data = data[sample(1:nrow(data), nrow(data)),]
  
  features <- freq2$features[c(freq2$Lasso>0 & freq2$RF>0 & freq2$RFERF>0 & freq2$RFESVM>0)]

  training_selected <- data[, c(features, "target")]
  validation_selected <- validation[, c(features, "target")]
  
  # train and evaluate models
  for (model_name in names(models_list)) {
    model_method <- models_list[[model_name]]
    
    # train
    if(model_name=="2ridge"){
      ridge_grid <- expand.grid(alpha = 0,lambda = seq(0, 0.1, by = 0.01))
      trained_model <- train(target ~ ., data = training_selected, method = model_method, trControl = trainControl(method = "cv", number = 3), tuneGrid = ridge_grid)
    }else{
      if(model_name=="3lasso"){
        trained_model <- train(target ~ ., data = training_selected, method = model_method, trControl = trainControl(method = "cv", number = 3))
      }else{trained_model <- train(target ~ ., data = training_selected, method = model_method, trControl = trainControl(method = "cv", number = 3))}}
    
    predictions_t <- predict(trained_model, training_selected[,-ncol(training_selected)])
    predictions_v <- predict(trained_model, validation_selected[,-ncol(validation_selected)])
    
    auc_t <- roc(as.numeric(training_selected$target),as.numeric(predictions_t))[["auc"]]
    auc_v <- roc(as.numeric(validation_selected$target),as.numeric(predictions_v))[["auc"]]
    
    re = data.frame(predictions = c(predictions_t,predictions_v), 
                    Actual =c(as.numeric(training_selected$target),as.numeric(validation_selected$target)),
                    predictions_type = c(rep("training", length(predictions_t)),rep("testing",length(predictions_v))),
                    AUC = c(rep(auc_t, length(predictions_t)),rep(auc_v, length(predictions_v)))) %>%
      mutate(Fold=i, Model = model_name)

    results <- rbind(results, re)
  }
}

write.csv(results,"selecting classifier.csv", row.names = F)


results = read.csv("selecting classifier.csv")
table(results$predictions_type)
performance = NULL
results$method = paste(results$Fold, results$features,results$Model, sep="_")
table(results$method)
table(results$predictions)

for(i in unique(results$method)){for(n in c("training","testing")){
      d = results %>% filter(method==i,predictions_type==n); table(d$method)
      if(nrow(d)==0){next}else{
        auc <- roc(as.numeric(d$Actual),as.numeric(d$predictions))[["auc"]]
        m <- confusionMatrix(as.factor(d$predictions), as.factor(d$Actual))
        re = cbind(as.data.frame(t(m[["overall"]])), as.data.frame(t(m[["byClass"]]))) %>% mutate(method=i, set=n, auc=auc)
        performance = rbind(performance, re)
    }
  }
}
table(results$predictions)
perf = performance
perf$model = gsub(".*\\_","",perf$method)
perf$Fold = gsub("\\_.*","",perf$method)
names(perf)
write.csv(performance,"performance of 9 classifiers.csv", row.names = F)


########################################################
perf = data.table::fread("performance of 9 classifiers.csv")
perf$model = gsub(".*\\_","",perf$method)
perf$Fold = gsub("\\_.*","",perf$method)
names(perf)
p = reshape2::melt(perf,id.vars=c(19,20,22,23), 
                   measure.vars=c("auc","Accuracy","F1","Sensitivity","Specificity","Pos Pred Value","Neg Pred Value"))

names(p)
p2 = p %>% group_by(model,set,variable) %>% 
  summarise(mean = mean(value), sd = sd(value))

ggplot(p2, aes(variable, mean, color=model, group=model, shape=model, lintype=model))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(4,5,15,16,17,9,8,26,29,13,14))+
  scale_color_manual(values = c("#f9a363","#3b4992","#3d7dae","#af322f","#8b7ec0","#8a4198","#725663","green4","black"))+
  scale_linetype_manual(values = c(1,2,2,1,2,2,1,1,1))+
  facet_wrap(.~set, ncol=3, scales = "free")+
  theme_classic()+
  scale_y_continuous(limits = c(0.5,1), expand = c(0.01,0.01))+
  theme(axis.text.x = element_text(angle = 90),
        strip.background = element_blank(),
        axis.line = element_line(linewidth=.5, color="black"),
        axis.ticks = element_line(linewidth=.5, color="black"),
        axis.ticks.length = unit(1, "mm"))
ggsave("Fig. 5c.pdf", width = 7, height = 3.5)





#################################################### final model
rm(list=ls())
d2 = read.csv("H:/20250330 figure+code/Figure 5/ML dataset 2 for machine learning.csv",row.names = 1)
colnames(d2)[2]="target"


d1 = read.csv("H:/20250330 figure+code/Figure 5/ML dataset 1 for machine learning.csv",row.names = 1)
table(d1$csDMARDs)
df = d1[,-c(1,3,4)]; colnames(df)[1]="target"
names(d1)
table(df$target)


i=1
training = df

################################ 
t_yes = training[training$target==1,]
set.seed(123); t_SMOTE <-  SMOTE(training[,2:ncol(training)],training[1],dup_size = 3, K=5)$data 
data1 = t_SMOTE[,c(ncol(t_SMOTE), 1:(ncol(t_SMOTE)-1))]; colnames(data1)[1] ="target"; table(data1$target)
data1 = data1 %>% filter(target==1)
training2 = rbind(training,t_yes,t_yes[sample(1:nrow(t_yes),15),])
set.seed(123); data = rbind(training2, data1[sample(1:nrow(data1),table(training2$target)["0"]-table(training2$target)["1"]),]); table(data$target)

set.seed(123); data = data[sample(1:nrow(data), nrow(data)),]


###############################
freq = read.csv("5-fold 4_features_selection results.csv") %>% 
  select(features, Fold, f) %>% distinct() %>% filter(Fold %in% c(1:5))

names(freq)
freq2 = reshape2::dcast(freq, features ~ f, value.var = "Fold") %>%
  arrange(desc(Lasso),desc(RF),desc(RFERF),desc(RFESVM))

features <- freq2$features[c(freq2$Lasso>0 & freq2$RF>0 & freq2$RFERF>0 & freq2$RFESVM>0)]
features
#####################################


# clinical indicators + selected citrullinated peptides
training_selected <- data[, c(features, "target")]
external_selected = d2[,c(features, "target")]
write.csv(training_selected[,c(7,1:6)],"training.csv")

# clinical indicators only
training_cli <- data[, c(features[features %in% c("TJC","DAS28.CRP.HM","SJC","CRP")], "target")]
external_cli = d2[,c(features[features %in% c("TJC","DAS28.CRP.HM","SJC","CRP")], "target")]



############################################################ prediction
# clinical indicators + selected citrullinated peptides
set.seed(1)
trained_model <- train(target ~ ., data = training_selected, method = "knn", trControl = trainControl(method = "cv", number = 3))
predictions_t <- predict(trained_model, training_selected[,-ncol(training_selected)], type = "prob")
predictions_e <- predict(trained_model, external_selected[,-ncol(external_selected)], type = "prob")
# calculate auc
auc_t <- roc(as.numeric(training_selected$target),as.numeric(predictions_t$`1`))[["auc"]]
auc_e <- roc(as.numeric(external_selected$target),as.numeric(predictions_e$`1`))[["auc"]]


# clinical indicators only
set.seed(1)
trained_cli <- train(target ~ ., data = training_cli, method = "knn", trControl = trainControl(method = "cv", number = 3))
predictions_t_cli <- predict(trained_cli, training_cli[,-ncol(training_cli)], type = "prob")
predictions_e_cli <- predict(trained_cli, external_cli[,-ncol(external_cli)], type = "prob")

auc_t_cli <- roc(as.numeric(training_cli$target),as.numeric(predictions_t_cli$`1`))[["auc"]]
auc_e_cli <- roc(as.numeric(external_cli$target),as.numeric(predictions_e_cli$`1`))[["auc"]]


pdf("Fig 5e.pdf", width = 3, height = 3)
plot.roc(as.numeric(external_selected$target),as.numeric(predictions_e$`1`),col = "#A31621", percent = TRUE, lwd = 2, print.auc = TRUE, print.auc.cex = 1, 
         print.auc.pattern = "cli+pep: %.1f%%", print.auc.y = 20, print.auc.x = 70, main = "")
plot.roc(as.numeric(external_cli$target),as.numeric(predictions_e_cli$`1`),col = "#3d7dae", percent = TRUE, lwd = 2, print.auc = TRUE, print.auc.cex = 1, 
         print.auc.pattern = "cli: %.1f%%", print.auc.y = 10, print.auc.x = 70, main = "", add=T)
dev.off()



excluded_feat = colnames(df)[!(colnames(df) %in% c(features,"CRP","SJC","target"))]

pdf("Fig 5f.pdf", width = 3, height = 3)
auc = NULL
plot.roc(as.numeric(external_selected$target),as.numeric(predictions_e$`1`),col = "transparent", percent = TRUE, lwd = 2, print.auc = TRUE, print.auc.cex = 1, 
         print.auc.pattern = "feature: %.1f%%", print.auc.y = 30, print.auc.x = 70, main = "cli + pep")
for(i in c(1,12,123,1234,12345)){
  set.seed(i); training_ran <- data[, c(features[features %in% c("TJC","DAS28.CRP.HM","SJC","CRP")],excluded_feat[sample(1:length(excluded_feat),ncol(training_selected) - ncol(training_cli))], "target")]
  external_ran = d2[, colnames(training_ran)]
  
  # 临床指标+随机肽段
  trained_ran <- train(target ~ ., data = training_ran, method = "glm", trControl = trainControl(method = "cv", number = 6))
  predictions_t_ran <- predict(trained_ran, training_ran[,-ncol(training_ran)], type = "prob")
  predictions_e_ran <- predict(trained_ran, external_ran[,-ncol(external_ran)], type = "prob")
  
  auc_t_ran <- roc(as.numeric(training_ran$target),as.numeric(predictions_t_ran$`1`))[["auc"]]
  auc_e_ran <- roc(as.numeric(external_ran$target),as.numeric(predictions_e_ran$`1`))[["auc"]]
  
  plot.roc(as.numeric(external_ran$target),as.numeric(predictions_e_ran$`1`),col = "#f9a363", percent = TRUE, lwd = 2, print.auc = TRUE, print.auc.cex = 1, 
           print.auc.pattern = "feature: %.1f%%", print.auc.y = 30, print.auc.x = 70, main = "cli + pep", add=T)
  auc = c(auc, auc_e_ran)
}
mean(auc); range(auc)
dev.off()




result = rbind(data.frame(Actual = as.numeric(external_selected$target),pred = as.numeric(predictions_e$`1`), type = "1_cli+pep"),
               data.frame(Actual = as.numeric(external_cli$target),pred = as.numeric(predictions_e_cli$`1`), type = "1_cli"))
for(i in c(1,12,123,1234,12345)){
  set.seed(i); training_ran <- data[, c(features[features %in% c("TJC","DAS28.CRP.HM","SJC","CRP")],excluded_feat[sample(1:length(excluded_feat),ncol(training_selected) - ncol(training_cli))], "target")]
  external_ran = d2[, colnames(training_ran)]
  
  # 临床指标+随机肽段
  trained_ran <- train(target ~ ., data = training_ran, method = "glm", trControl = trainControl(method = "cv", number = 10))
  predictions_t_ran <- predict(trained_ran, training_ran[,-ncol(training_ran)], type = "prob")
  predictions_e_ran <- predict(trained_ran, external_ran[,-ncol(external_ran)], type = "prob")
  
  result = rbind(result,data.frame(Actual = as.numeric(external_ran$target),pred = as.numeric(predictions_e_ran$`1`), type = paste("cli_random",i)))
}
table(result$type)

performance=NULL
for(n in c("1_cli","1_cli+pep","cli_random 1","cli_random 12","cli_random 123","cli_random 1234","cli_random 12345")){
    d = result %>% filter(type==n)
    if(nrow(d)==0){next}else{
      d$class = ifelse(d$pred >= 0.5,1,0)
      m <- confusionMatrix(as.factor(d$class), as.factor(d$Actual))
      re = cbind(as.data.frame(t(m[["overall"]])), as.data.frame(t(m[["byClass"]]))) %>% mutate(set=n)
      performance = rbind(performance, re)
  }
}
table(performance$set)
p1 = reshape2::melt(performance, id.vars=c(19), measure.vars=c("Accuracy","F1","Sensitivity","Specificity","Pos Pred Value","Neg Pred Value"))
p1$group = gsub("\\ .*","",p1$set); table(p1$group)
p2 = p1 %>% group_by(group,variable) %>% summarise(mean=mean(value), sd=sd(value))
p2$sd[is.na(p2$sd)]=0
ggplot(p2, aes(variable, mean, color=group, group=group, shape=group))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin = mean-sd, ymax= mean+sd), width=.2)+
  scale_shape_manual(values = c(15,16,17,9,8,26,29,13,14))+
  scale_color_manual(values = c("#3d7dae","#af322f","#f9a363","#3b4992","#8b7ec0","#8a4198","#725663","green4","black"))+
  #facet_wrap(.~set, ncol=3, scales = "free")+
  theme_classic()+
  scale_y_continuous(limits = c(0.48,1), expand = c(0.01,0.01))+
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major = element_line(linetype=3, color="gray50"),
        strip.background = element_blank(),
        axis.line = element_line(linewidth=.33, color="black"),
        axis.ticks = element_line(linewidth=.33, color="black"),
        axis.ticks.length = unit(0.8, "mm"))
ggsave("Fig. 5h.pdf", width = 3.5, height = 3)


library(yardstick)
library(ggplot2)
library(pROC)
library(caret)
result$pred = ifelse(result$pred>0.5,"1","0")
result$Actual = as.factor(result$Actual)
result$pred = as.factor(result$pred)
data = result %>% filter(type=="1_cli+pep")
cm <- conf_mat(data, Actual, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")
ggsave("Fig. 5g(up) cli+pep.pdf", height = 2, width = 2)


data = result %>% filter(type=="1_cli")
cm <- conf_mat(data, Actual, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")
ggsave("Fig. 5g(middle) cli.pdf", height = 2, width = 2)


# selected the median one
auc
table(result$type)
data = result %>% filter(type=="cli_random 1")
cm <- conf_mat(data, Actual, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")
ggsave("Fig. 5g(down) cli+random_median.pdf", height = 2, width = 2)












