# The below packages must be installed in order to run the code
#install.packages('corrplot')
#install.packages('sqdlf')
#install.packages('class')
#install.packages('caret')
#install.packages('e1071')
#install.packages('gmodels')

# Loading the dataset
heart<-read.csv("Heart.csv", header = FALSE)
head(heart)
# Renaming the Column Names
names(heart)<-c("age","sex","chest_pain","resting_bp","cholestrol","fasting_blood_sugar","resting_ecg","max_heart_rate","exercise_induced_angina","oldpeak","slope_of_peak_exercise","number of major vessels colored","thallium heart scan","result")
View(heart)

########  Detecting and removing NA values  #######

heart[heart=="?"]<-NA  # Replacing '?' wiht 'NA'
nrow(heart[is.na(heart$`number of major vessels colored`) | is.na(heart$`thallium heart scan`),])
nrow(heart)
data <- heart[!(is.na(heart$`number of major vessels colored`) | is.na(heart$`thallium heart scan`)),]
nrow(data)

######## Converting to numeric form ##########
data$age <- as.numeric(data$age)
data$`chest_pain` <- as.numeric(data$`chest_pain`)
data$`fasting_blood_sugar` <- as.numeric(data$`fasting_blood_sugar`)
data$`resting_ecg` <- as.numeric(data$`resting_ecg`)
data$`exercise_induced_angina` <- as.numeric(data$`exercise_induced_angina`)
data$`slope_of_peak_exercise` <- as.numeric(data$`slope_of_peak_exercise`)
data$`resting_bp` <- as.numeric(data$`resting_bp`)
data$`thallium heart scan` <- as.numeric(data$`thallium heart scan`)
data$cholestrol <- as.numeric(data$cholestrol)
data$sex <- as.numeric(data$sex)
data$`number of major vessels colored` <- as.numeric(data$`number of major vessels colored`)

#View(data)

########## K N N Classification ############

# Creating a function to normalize the data
normalize <- function(x) {
        return((x - min(x)) / (max(x) - min(x)))
}

# storing the normalized data in 'data_n' using 'lapply' for columns 1 to 13
data_n <- as.data.frame(lapply(data[1:13], normalize))
summary(data_n)

# setting seed to get the same set of random data 
set.seed(1000)

# random selection of 70% of data using sample()      
rand.70 <- sample(1:nrow(data_n),size=nrow(data_n)*0.7,replace = FALSE)

# Training set
train_set <- data_n[rand.70,]   # 70% training data
test_set <- data_n[-rand.70,]   # 30% test data

# Target set
# Creating a data frame for 'defaulter' feature which is our result
train_target <- data[rand.70,14]
test_target <- as.factor(data[-rand.70,14])

#install.packages('class')
library('class')
sqrt(297) # total observations are 297
knn.17 <- as.factor(knn(train = train_set, test = test_set, cl = train_target, k = 17))
table(knn.17, test_target)
ACC.173 <- 100 * sum(test_target == knn.17)/NROW(test_target)
ACC.173  

# obtaining the cross table
#install.packages('gmodels')
library('gmodels')
CrossTable(x = test_target, y = knn.17, prop.chisq = FALSE)

# obtaining the confusion matrix
#install.packages('caret')
#install.packages('e1071')
library('caret')
library('e1071')
confusionMatrix(test_target , knn.17)

############### classification ends ################

female<-subset(data,sex==0)
male<-subset(data,sex=1)

#For females

logistic_female <- glm(result ~ ., data=female, family="binomial")
summary(logistic_female)

#For males
logistic_male<-glm(result~., data=male, family= "binomial")
summary(logistic_male)

########## scatter-plot matrix ##########

pairs(~chest_pain +  max_heart_rate +  result, data=data)


#######Correlation matrix ############

#install.packages('corrplot')
#install.packages('sqdlf')
library('corrplot')
library('sqldf')
#names(data)
#str(data)
data1<-sqldf("SELECT age as Age, sex as Sex, chest_pain as CP, resting_bp as RBP, 
             cholestrol as Cholestrol, fasting_blood_sugar as FBS, resting_ecg as ECG,
             max_heart_rate as HR, exercise_induced_angina as Angina, result as res FROM data")

corMatrix <- cor(data1)

########Correlation matrix##########

corrplot(corMatrix)
par(mfrow=c(1,1))
corrplot(corMatrix, method="circle", type="lower", addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         diag=TRUE, sig.level = 0.05, insig = "blank")


