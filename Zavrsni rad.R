#####################
###Obrada podataka###
#####################

#Ucitavanje .csv file-a

dipl <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors = FALSE)
dipl.original <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors = FALSE)

#ucitavanje paketa

library(ggplot2)

library(caret)

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

library(e1071)

library(bnlearn)

library(pROC)

library(randomForest)

#funkcija za evaluacione metrike

compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}


#korekcija imena varijable ï..Age u Age
names(dipl)[names(dipl) == 'ï..Age'] <- 'Age'

#brisanje varijabli koje nisu znacajne za predikciju

# 1) sve vrednosti su 1 za EmployeeCount
unique(dipl$EmployeeCount) 
dipl$EmployeeCount<- NULL

# 2) sve vrednosti su Y za Over18
unique(dipl$Over18) 
dipl$Over18<- NULL

# 3) var predstavlja redne brojeve zaposlenih
unique(dipl$EmployeeNumber)
dipl$EmployeeNumber <- NULL

# 4) sve vrednosti su 80
unique(dipl$StandardHours)
dipl$StandardHours<- NULL

#varijable cije znacenje nije potpuno jasno (nigde nije objasnjeno sta one predstavljaju niti se to moze 
# zakljuciti u odnosu na ostale varijable ) 

dipl$DailyRate<-NULL
dipl$HourlyRate<-NULL
dipl$MonthlyRate<-NULL

#Analiza svake od preostalih varijabli 

###KATEGORICKE VARIJABLE###

#Attrition 
dipl$Attrition <- factor(dipl$Attrition)
levels(dipl$Attrition)

table(dipl$Attrition)

#BusinessTravel (Dobra var za predikciju)
dipl$BusinessTravel <- factor(dipl$BusinessTravel)
levels(dipl$BusinessTravel)

table(dipl$BusinessTravel)

ggplot(data = dipl, mapping = aes(x=BusinessTravel, fill=Attrition))+geom_bar(position = "dodge")

#Department (Dobra var za predikciju)
dipl$Department <- factor(dipl$Department)
levels(dipl$Department)

table(dipl$Department)

ggplot(data = dipl, mapping = aes(x=Department, fill=Attrition))+geom_bar(position = "dodge")

#Education (Dobra var za predikciju) 1 'Below College' 2 'College' 3 'Bachelor' 4 'Master' 5 'Doctor'
dipl$Education <-factor(dipl$Education)
levels(dipl$Education)

table(dipl$Education)

ggplot(data = dipl, mapping = aes(x=Education, fill=Attrition))+geom_bar(position = "dodge")

#EducationField (Dobra var za predikciju)
dipl$EducationField <- factor(dipl$EducationField)
levels(dipl$EducationField)

table(dipl$EducationField)

ggplot(data = dipl, mapping = aes(x=EducationField, fill=Attrition))+geom_bar(position = "dodge")

#EnvironmentSatisfaction (Dobra var za predikciju) 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
dipl$EnvironmentSatisfaction<- factor(dipl$EnvironmentSatisfaction)
levels(dipl$EnvironmentSatisfaction)

table(dipl$EnvironmentSatisfaction)

ggplot(data = dipl, mapping = aes(x=EnvironmentSatisfaction, fill=Attrition))+geom_bar(position = "dodge")

#Gender (Dobra var za predikciju)
dipl$Gender <- factor(dipl$Gender)
levels(dipl$Gender)

table(dipl$Gender)

ggplot(data = dipl, mapping = aes(x=Gender, fill=Attrition))+geom_bar(position = "dodge")

#JobInvolvment (Dobra var za predikciju) 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
dipl$JobInvolvement <- factor(dipl$JobInvolvement)

levels(dipl$JobInvolvement)

table(dipl$JobInvolvement)

ggplot(data = dipl, mapping = aes(x=JobInvolvement, fill=Attrition))+geom_bar(position = "dodge")

#JobLevel (Dobra var za predikciju)
dipl$JobLevel <- factor(dipl$JobLevel)

levels(dipl$JobLevel)

table(dipl$JobLevel)

ggplot(data = dipl, mapping = aes(x=JobLevel, fill=Attrition))+geom_bar(position = "dodge")

#JobRole  (Dobra var za predikciju)
dipl$JobRole <- factor(dipl$JobRole)
levels(dipl$JobRole)

table(dipl$JobRole)

ggplot(data = dipl, mapping = aes(x=JobRole, fill=Attrition))+geom_bar(position = "dodge")

#JobSatisfaction (Dobra var za predikciju) 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
dipl$JobSatisfaction <- factor(dipl$JobSatisfaction)

levels(dipl$JobSatisfaction)

table(dipl$JobSatisfaction)

ggplot(data = dipl, mapping = aes(x=JobSatisfaction, fill=Attrition))+geom_bar(position = "dodge")
#Proportion plot
ggplot(data = data.frame(prop.table(table(JobSatisfaction = dipl$JobSatisfaction,Attrition = dipl$Attrition), margin = 1)), mapping = aes(x=JobSatisfaction, y= Freq,  fill=Attrition))+ylab("Proportion")+geom_col(position = "dodge")

#MaritalStatus (Dobra var za predikciju)
dipl$MaritalStatus <- factor(dipl$MaritalStatus)

levels(dipl$MaritalStatus)

table(dipl$MaritalStatus)

ggplot(data = dipl, mapping = aes(x=MaritalStatus, fill=Attrition))+geom_bar(position = "dodge")

#NumCompaniesWorked (Dobra var za predikciju) 20% ljudi je radilo u 5 ili vise kompanija pa ce oni dobiti vrednost 5+
table(dipl$NumCompaniesWorked)

dipl$NumCompaniesWorked[dipl$NumCompaniesWorked > 5] <- 5

table(dipl$NumCompaniesWorked)

dipl$NumCompaniesWorked <- factor(dipl$NumCompaniesWorked, levels = c(0,1,2,3,4,5), labels = c("0","1","2","3","4","5+"))

levels(dipl$NumCompaniesWorked)

table(dipl$NumCompaniesWorked)
prop.table(table(dipl$NumCompaniesWorked))

ggplot(data = dipl, mapping = aes(x=NumCompaniesWorked, fill=Attrition))+geom_bar(position = "dodge")

#OverTime (Dobra var za predikciju)
dipl$OverTime <- factor(dipl$OverTime)

levels(dipl$OverTime)

table(dipl$OverTime)

ggplot(data = dipl, mapping = aes(x=OverTime, fill=Attrition))+geom_bar(position = "dodge")

#Proportion plot
ggplot(data = data.frame(prop.table(table(OverTime = dipl$OverTime,Attrition = dipl$Attrition), margin = 1)), mapping = aes(x=OverTime, y= Freq,  fill=Attrition))+ ylab("Proportion") +geom_col(position = "dodge")



#PerformanceRating   (Nije dobra var za predikciju, uklonjena) 1 'Low' 2 'Good' 3 'Excellent' 4 'Outstanding'
dipl$PerformanceRating<- factor(dipl$PerformanceRating)

levels(dipl$PerformanceRating)

table(dipl$PerformanceRating)

ggplot(data = dipl, mapping = aes(x=PerformanceRating, fill=Attrition))+geom_bar(position = "dodge")

#Proportion plot
ggplot(data = data.frame(prop.table(table(PerformanceRating = dipl$PerformanceRating,Attrition = dipl$Attrition), margin = 1)), mapping = aes(x=PerformanceRating, y= Freq,  fill=Attrition))+ylab("Percentage")+geom_col(position = "dodge")


#RelationshipSatisfaction  (Nije dobra var za predikciju, uklonjena) 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
dipl$RelationshipSatisfaction<- factor(dipl$RelationshipSatisfaction)

levels(dipl$RelationshipSatisfaction)

table(dipl$RelationshipSatisfaction)

ggplot(data = dipl, mapping = aes(x=RelationshipSatisfaction, fill=Attrition))+geom_bar(position = "dodge")

#Proportion plot
ggplot(data = data.frame(prop.table(table(RelationshipSatisfaction = dipl$RelationshipSatisfaction,Attrition = dipl$Attrition), margin = 1)), mapping = aes(x=RelationshipSatisfaction, y= Freq,  fill=Attrition))+ylab("Percentage")+geom_col(position = "dodge")


#StockOptionLevel (Dobra var za predikciju)

dipl$StockOptionLevel<- factor(dipl$StockOptionLevel)

levels(dipl$StockOptionLevel)

table(dipl$StockOptionLevel)

ggplot(data = dipl, mapping = aes(x=StockOptionLevel, fill=Attrition))+geom_bar(position = "dodge")

#TrainingTimesLastYear (Dobra var za predikciju) 1= below average, 2 = average, 3 = above average
table(dipl$TrainingTimesLastYear)

dipl$TrainingTimesLastYear[dipl$TrainingTimesLastYear < 2] <- 1
dipl$TrainingTimesLastYear[dipl$TrainingTimesLastYear > 3] <- 4
dipl$TrainingTimesLastYear[dipl$TrainingTimesLastYear == 3] <- 2
dipl$TrainingTimesLastYear[dipl$TrainingTimesLastYear == 4] <- 3


table(dipl$TrainingTimesLastYear)

dipl$TrainingTimesLastYear <- factor(dipl$TrainingTimesLastYear, levels = c(1,2,3), labels = c("1","2","3"))

levels(dipl$TrainingTimesLastYear)

table(dipl$TrainingTimesLastYear)
prop.table(table(dipl$TrainingTimesLastYear))

ggplot(data = dipl, mapping = aes(x=TrainingTimesLastYear, fill=Attrition))+geom_bar(position = "dodge")



#WorkLifeBalance (Dobra var za predikciju) 1 'Bad' 2 'Good' 3 'Better' 4 'Best'
dipl$WorkLifeBalance <- factor(dipl$WorkLifeBalance)

levels(dipl$WorkLifeBalance)

table(dipl$WorkLifeBalance)

ggplot(data = dipl, mapping = aes(x=WorkLifeBalance, fill=Attrition))+geom_bar(position = "dodge")


###NUMERICKE VARIJABLE###


#Age (Dobra var za predikciju)
ggplot(data = dipl,mapping = aes(x = dipl$Age, fill = dipl$Attrition)) + 
  geom_density(alpha = 0.5)+ xlab("Age")+ scale_fill_discrete(name="Attrition")



#DistanceFromHome (Dobra var za predikciju)
ggplot(data = dipl,mapping = aes(x = dipl$DistanceFromHome, fill = dipl$Attrition)) + 
  geom_density(alpha = 0.5)+ xlab("DistanceFromHome")+ scale_fill_discrete(name="Attrition")



#MonthlyIncome (Dobra var za predikciju)
ggplot(data = dipl,mapping = aes(x = dipl$MonthlyIncome, fill = dipl$Attrition)) + 
  geom_density(alpha = 0.5)+ xlab("MonthlyIncome")+ scale_fill_discrete(name="Attrition")



#PercentSalaryHike (Nije Dobra var za predikciju)
ggplot(data = dipl,mapping = aes(x = dipl$PercentSalaryHike, fill = dipl$Attrition)) + 
  geom_density(alpha = 0.5)+ xlab("PercentSalaryHike")+ scale_fill_discrete(name="Attrition")



#TotalWorkingYears (Dobra var za predikciju) 
ggplot(data = dipl,mapping = aes(x = dipl$TotalWorkingYears, fill = dipl$Attrition)) + 
  geom_density(alpha = 0.5)+ xlab("TotalWorkingYears")+ scale_fill_discrete(name="Attrition")



#YearsAtCompany (Dobra var za predikciju)
ggplot(data = dipl,mapping = aes(x = dipl$YearsAtCompany, fill = dipl$Attrition)) + 
  geom_density(alpha = 0.5)+ xlab("YearsAtCompany")+ scale_fill_discrete(name="Attrition")



#YearsInCurrentRole (Dobra var za predikciju)
ggplot(data = dipl,mapping = aes(x = dipl$YearsInCurrentRole, fill = dipl$Attrition)) + 
  geom_density(alpha = 0.5)+ xlab("YearsInCurrentRole")+ scale_fill_discrete(name="Attrition")



#YearsSinceLastPromotion (Nije Dobra var za predikciju)
ggplot(data = dipl,mapping = aes(x = dipl$YearsSinceLastPromotion, fill = dipl$Attrition)) + 
  geom_density(alpha = 0.5)+ xlab("YearsSinceLastPromotion")+ scale_fill_discrete(name="Attrition")



#YearsWithCurrManager (Dobra var za predikciju)
ggplot(data = dipl,mapping = aes(x = dipl$YearsWithCurrManager, fill = dipl$Attrition)) + 
  geom_density(alpha = 0.5)+ xlab("YearsWithCurrManager")+ scale_fill_discrete(name="Attrition")


###UKLANJANJE VARIJABLI###


#var koje nemaju dovoljan prediktivni potenicjal
dipl$PercentSalaryHike<-NULL
dipl$YearsSinceLastPromotion<-NULL 
dipl$PerformanceRating<- NULL
dipl$RelationshipSatisfaction <- NULL


###################
###Decision Tree###
###################

#dipl.DT kopija dipl Data seta koji ce se koristiti za kreiranje modela Decision tree 
dipl.DT <- dipl

#Podela Data seta dipl.DT na train i test deo 80% opservacija ulazi u train data set, u daljem radu koriscen je set seed(10)
set.seed(10)
train.indices <- createDataPartition(dipl.DT$Attrition, p= .80, list = FALSE)
dipl.DT.train <- dipl.DT[train.indices,]
dipl.DT.test <- dipl.DT[-train.indices,]
#I train i test data set sadrze priblizan procenat Yes i No vrednosti varijable 
prop.table(table(dipl.DT.train$Attrition))
prop.table(table(dipl.DT.test$Attrition))

###Decision tree 1. verzija bez CP parametra### 

tree1 <- rpart(Attrition ~ ., data = dipl.DT.train, method = "class")
print(tree1)

fancyRpartPlot(tree1)#plot decision tree 1


tree1$variable.importance#var importance za svaku koriscenu var u kreiranju decision tree 1

#predikcija nad test data setom
tree1.pred <- predict(object = tree1, newdata = dipl.DT.test, type = "class")
tree1.pred[1:10]

#matruca konfuzije za tree1
tree1.cm <- table(true = dipl.DT.test$Attrition, predicted=tree1.pred)
tree1.cm 

#evalacija 
tree1.eval <- compute.eval.metrics(tree1.cm)
tree1.eval


###Decision tree 2. verzija sa optimalnom vrednoscu CP parametra### 

#Cross-validation
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by= 0.001))

set.seed(10)
dt.cv <- train(Attrition ~ .,
               data = dipl.DT.train,
               method = "rpart",
               control = rpart.control(minsplit = 10),
               trControl = numFolds,
               tuneGrid = cpGrid)
dt.cv

plot(dt.cv)

#Decision tree 2
tree2<-prune(tree1, cp = 0.026)
fancyRpartPlot(tree2)#plot decision tree 2

tree2$variable.importance#var importance za svaku koriscenu var u kreiranju decision tree 2

#predikcija nad test data setom
tree2.pred <- predict(object = tree2, newdata = dipl.DT.test, type = "class")
tree2.pred[1:10]

#matruca konfuzije za tree2
tree2.cm <- table(true = dipl.DT.test$Attrition, predicted=tree2.pred)
tree2.cm 
tree1.cm

#evalacija 
tree2.eval <- compute.eval.metrics(tree2.cm)
tree2.eval
tree1.eval


#################
###Naive Bayes###
#################

#dipl.NB kopija dipl Data seta koji ce se koristiti za kreiranje modela Naive bayes 
dipl.NB <- dipl


#Shapiro test kako bi smo proverili da li numericke varijable imaju normalnu raspodelu -> ni jedna num var nema normalnu raspodelu

num.vars.NB<- c(1,5,15,19,22,23,24)
apply(X = dipl.NB[,num.vars.NB], MARGIN = 2, FUN = shapiro.test)

#konverzija iz int u num tip jer discretize funkcija radi samo sa numerickim var.
dipl.NB$Age <- as.numeric(dipl.NB$Age)
dipl.NB$DistanceFromHome <- as.numeric(dipl.NB$DistanceFromHome)
dipl.NB$MonthlyIncome <- as.numeric(dipl.NB$MonthlyIncome)
dipl.NB$TotalWorkingYears <- as.numeric(dipl.NB$TotalWorkingYears)
dipl.NB$YearsAtCompany <- as.numeric(dipl.NB$YearsAtCompany)
dipl.NB$YearsInCurrentRole <- as.numeric(dipl.NB$YearsInCurrentRole)
dipl.NB$YearsWithCurrManager <- as.numeric(dipl.NB$YearsWithCurrManager)

#diskretizacija numerickih varijabli
to.discretize.NB <- c("Age", "DistanceFromHome", "MonthlyIncome", "TotalWorkingYears", "YearsAtCompany", "YearsInCurrentRole", "YearsWithCurrManager")
discretized.NB <- discretize(data = dipl.NB[,to.discretize.NB],
                             method = 'quantile',
                             breaks = c(5,5,5,5,5,5,5))

summary(discretized.NB)

#plot za svaku od varijabli
#Age 
summary(discretized.NB$Age)
ggplot(data = discretized.NB, mapping = aes(x = Age)) +
  geom_bar()

#DistanceFromHome
summary(discretized.NB$DistanceFromHome)
ggplot(data = discretized.NB, mapping = aes(x = DistanceFromHome)) +
  geom_bar()

#MonthlyIncome
summary(discretized.NB$MonthlyIncome)
ggplot(data = discretized.NB, mapping = aes(x = MonthlyIncome)) +
  geom_bar()


#TotalWorkingYears 
summary(discretized.NB$TotalWorkingYears)
ggplot(data = discretized.NB, mapping = aes(x = TotalWorkingYears)) +
  geom_bar()


#YearsAtCompany 
summary(discretized.NB$YearsAtCompany)
ggplot(data = discretized.NB, mapping = aes(x = YearsAtCompany)) +
  geom_bar()

#YearsInCurrentRole 
summary(discretized.NB$YearsInCurrentRole)
ggplot(data = discretized.NB, mapping = aes(x = YearsInCurrentRole)) +
  geom_bar()

#YearsWithCurrManager 
summary(discretized.NB$YearsWithCurrManager)
ggplot(data = discretized.NB, mapping = aes(x = YearsWithCurrManager)) +
  geom_bar()

#izrada nove promenljive dipl.NB.new sa svim numerickim var koje su sada diskretizovane
cols.to.add <- setdiff(names(dipl.NB), names(discretized.NB))
dipl.NB.new <- data.frame(cbind(dipl.NB[,cols.to.add], discretized.NB))
str(dipl.NB.new)
dipl.NB.new <- dipl.NB.new[,names(dipl.NB)]
str(dipl.NB.new)

#Podela Data seta dipl.NB.new na train i test deo 80% opservacija ulazi u train data set, u daljem radu koriscen je set seed(10)
set.seed(10)
train.indices.NB <- createDataPartition(dipl.NB.new$Attrition, p = 0.8, list = FALSE)
dipl.NB.train <- dipl.NB.new[train.indices.NB,]
dipl.NB.test <- dipl.NB.new[-train.indices.NB,]

#kreiranje modela sa svim var 

nb1 <- naiveBayes(Attrition ~ ., data = dipl.NB.train)
print(nb1)

#predikcija sa podrazumevanim treshold-om
nb1.pred <- predict(nb1, newdata = dipl.NB.test, type = 'class')
head(nb1.pred)
table(nb1.pred)

#Matrica konfuzije MC

nb1.cm <- table(true = dipl.NB.test$Attrition, predicted = nb1.pred)
nb1.cm


#evaluacija preko ucitane funkije

nb1.eval <- compute.eval.metrics(nb1.cm)
nb1.eval

#Model naive Bayes sa odabranim var iz drveta odlucivanja Overtime i monthlyIncome

nb2 <- naiveBayes(Attrition ~ OverTime + MonthlyIncome, data = dipl.NB.train)
print(nb2)


#vrsenje predikcije
#prag je po defaultu = 0,5

nb2.pred <- predict(nb2, newdata = dipl.NB.test, type = 'class')
head(nb2.pred)
table(nb2.pred)

#matrica konfuzije CM
nb2.cm <- table(true = dipl.NB.test$Attrition, predicted = nb2.pred)
nb2.cm
nb1.cm

#evaluacija preko ucitane  funkije

nb2.eval <- compute.eval.metrics(nb2.cm)
nb2.eval
nb1.eval

#Naive Bayes sa odredjenim pragom (treshold) pomocu ROC krive
nb2.pred.prob <- predict(nb2, newdata = dipl.NB.test, type = "raw") 
head(nb2.pred.prob)

#ROC kriva
nb2.roc <- roc(response = as.numeric(dipl.NB.test$Attrition),
               predictor = nb2.pred.prob[,1],
               levels = c(2, 1))

#plotovanje ROC krive
#youden - maksimizira vrednost specificity + sensitivity (podjednaka vaznost tacnosti predvidjanja pozitivne i negativne klase)
plot.roc(nb2.roc,
         print.thres = TRUE,
         print.thres.best.method = "youden")

#Maximumi za acc spec, sens, i vrednost thr
nb2.coords <- coords(nb2.roc,
                     ret = c("accuracy", "spec", "sens", "thr"),
                     x = "local maximas")

nb2.coords
# najbolja vrednost za thteshold je 0.8527147 jer je najbolji odnos specificity-ja (0.7021277) i sensitivity-ja (0.6056911) u toj tacki ROC krive
prob.threshold <- nb2.coords[4,7]

#dodeljivanje klase Yes ili No zavisno od verovatnoce i 
nb2.pred2 <- ifelse(test = nb2.pred.prob[,1] >= prob.threshold, 
                    yes = "No", 
                    no = "Yes") 
nb2.pred2 <- as.factor(nb2.pred2)


nb2.cm2 <- table(actual = dipl.NB.test$Attrition, predicted = nb2.pred2)
nb2.cm2
nb2.cm
nb1.cm

nb2.eval2 <- compute.eval.metrics(nb2.cm2)
nb2.eval2
nb2.eval
nb1.eval




###################
###Random Forest###
###################

#dipl.RF kopija dipl Data seta koji ce se koristiti za kreiranje modela Random forest 
dipl.RF <- dipl

#Podela Data seta dipl.RF na train i test deo 80% opservacija ulazi u train data set
set.seed(10)
train.indices.RF <- createDataPartition(dipl.RF$Attrition, p= .80, list = FALSE)
dipl.RF.train <- dipl.RF[train.indices,]
dipl.RF.test <- dipl.RF[-train.indices,]

#kreiranje modela random forest sa svim defaultnim vrednostima (mtry = 4, ntree= 500)

forest <- randomForest(Attrition ~ ., data = dipl.RF.train)
forest.importance <- forest$importance

#plot for var improtance
varImpPlot(forest)

forest.pred <- predict(forest, newdata = dipl.RF.test, type = "class")
confusionMatrix(forest.pred, dipl.RF.test$Attrition)


forest.cm <- table(true = dipl.RF.test$Attrition, predicted = forest.pred)
forest.cm

forest.eval<- compute.eval.metrics(forest.cm)
forest.eval
  
#najbolja vrednost za mtry=4 kao i defaultna vrednost
set.seed(10)
bestmtry <- tuneRF(dipl.RF.train[,-2], dipl.RF.train[,2], stepFactor=1.5)

set.seed(10)
forest2 <- randomForest(Attrition ~ ., data=dipl.RF.train, importance=TRUE, ntree=2000)
forest

varImpPlot(forest2)
varImpPlot(forest)

forest2.pred <- predict(forest2, dipl.RF.test)

confusionMatrix(forest2.pred, dipl.RF.test$Attrition)

forest2.cm <- table(true = dipl.RF.test$Attrition, predicted = forest2.pred)
forest2.cm
forest.cm

forest2.eval<-  compute.eval.metrics(forest2.cm)
forest2.eval
forest.eval
