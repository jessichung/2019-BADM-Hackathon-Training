#Install 445 packages
install.packages("tidyverse")
install.packages("car")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("nnet")
install.packages("randomForest")
install.packages("effects")

# Load required R packages
library("tidyverse")
library("car")
library("rpart")
library("rpart.plot")
library("nnet")
library("foreign")
library("corrplot")
library("randomForest")
library("effects")
source("C:\\Users\\Jessica\\Downloads\\BCA_functions_source_file20191031.R") #Functions used in the class BUS 445

#Import dataset - Locations will be different for everyone
library(readr)
Retention2017 <- read_csv("BADM Hackathon/Training Day/Retention2017R_V2.csv")
view(Retention2017)

#PART 1 DATA CLEANING

#Variable summary to understand missing values
variable.summary(Retention2017)

#Convert to dates from characters to dates 
Retention2017$created <- as.Date(Retention2017$created,format = "%d/%m/%Y") 
Retention2017$firstorder <- as.Date(Retention2017$firstorder,format = "%d/%m/%Y")
Retention2017$lastorder <- as.Date(Retention2017$lastorder,format = "%d/%m/%Y")

#HANDLE MISSING VALUES

#eclickrate - eclickrate can be NA if the customer never received emails or received emails that never had links. 
#Therefore, it would be safe to assign these values to 0. As well, we should mark down which record was changed for future reference via an NA column.
Retention2017 <- mutate(Retention2017, NA_eclickrate = ifelse(is.na(eclickrate) == 1,1,0))
Retention2017$eclickrate[Retention2017$NA_eclickrate == 1] <- 0

#Refill and doorstep - NA values occur in the same record. Therefore it is assumed that customers were not offered this option and we can replace the values with N. As well, we should mark down which record was changed for future reference via an NA column.
Retention2017 <- mutate(Retention2017, NA_refill_doorstep = ifelse(is.na(refill) == 1,1,0))
Retention2017$refill[Retention2017$NA_refill_doorstep == 1] <- "N"
Retention2017$doorstep[Retention2017$NA_refill_doorstep == 1] <- "N"

#firstorder and lastorder - There are NA values now after the conversion. Viewing the dates columns in Excel also shows that there are some with dates such as 1/0/00 which is why they are NA when converted 
subset(Retention2017, is.na(Retention2017$firstorder))
subset(Retention2017, is.na(Retention2017$lastorder))
#50-51 \\REMOVE FOR CHEAT SHEET//

#Based on variable.summary, they represent less than 3% of the data with firstorder(0.056%) and lastorder(0.110%)
#Delete these rows from the dataset
Retention2017 <- Retention2017[!is.na(Retention2017$firstorder),]
Retention2017 <- Retention2017[!is.na(Retention2017$lastorder),]

#Review missing values again
variable.summary(Retention2017)

#Skip conversions on cheatsheet
#Convert binary variables from character to integer. If else relies on the fact that there are only Y and N values and nothing else
Retention2017$paperless <- ifelse(Retention2017$paperless == "Y",1,0)
Retention2017$refill <- ifelse(Retention2017$refill == "Y",1,0)
Retention2017$doorstep <- ifelse(Retention2017$doorstep == "Y",1,0)

#Convert categorical variables from character to factor
Retention2017$favday <- as.factor(Retention2017$favday)
Retention2017$city <- as.factor(Retention2017$city)
Retention2017$Sample <- as.factor(Retention2017$Sample)

#CHANGE TARGET TO FACTOR
Retention2017$lost <- as.factor(Retention2017$lost)

#PART 2 MAKE NEW VARIABLES
Retention2017$cust_period = as.numeric(Retention2017$lastorder - Retention2017$firstorder)

#PART 3 DEVELOP MODELS

#Tree Model
Model1.RPart <- rpart(formula = lost~
                        eopenrate
                        +eclickrate
                        +avgorder
                        +ordfreq
                        +paperless
                        +refill
                        +doorstep
                        +favday
                        +city
                        +cust_period,
                        data = filter(Retention2017, Sample == "Estimation"),
                        cp = 0.01,
                        model = TRUE
                      )
#Understand complexity paramater
plotcp(Model1.RPart)
printcp(Model1.RPart)

#Plot the model
rpart.plot(Model1.RPart,
           type = 0,
           fallen.leaves = TRUE,
           uniform = TRUE,
           yes.text = "TRUE",
           no.text = "FALSE",
           cex = .8
)

#Logisic Regression Model
Model2.LogReg <- glm(formula = lost ~ 
                         eopenrate
                         +eclickrate
                         +avgorder
                         +ordfreq
                         +paperless
                         +refill
                         +doorstep
                         +favday
                         +city
                         +cust_period,
                        data = filter(Retention2017, Sample == "Estimation"),
                        family = binomial(logit)
                     )
#Print summary of Model2.LogReg
summary(Model2.LogReg)

#Stepwise Regression Modification of Model2.LogReg
Model3.StepReg <- step(Model2.LogReg, direction = "both")
#Print summary of Model3.StepReg
summary(Model3.StepReg)

#Neural Network Model

Model4.NeuNet <- Nnet(formula = lost ~ 
                        eopenrate
                        +eclickrate
                        +avgorder
                        +ordfreq
                        +paperless
                        +refill
                        +doorstep
                        +favday
                        +city
                        +cust_period,
                       data = filter(Retention2017, Sample == "Estimation"),
                       decay = 0.10, # decay parameter
                       size = 2
                      ) 
#Final Objective Function Value
Model4.NeuNet$value
#View calibration coefficients
summary(Model4.NeuNet)

Model5.RanFor <- randomForest(formula = lost ~ 
                              eopenrate
                              +eclickrate
                              +avgorder
                              +ordfreq
                              +paperless
                              +refill
                              +doorstep
                              +favday
                              +city
                              +cust_period,
                              data = filter(Retention2017, Sample == "Estimation"),
                              importance = TRUE,
                              ntree = 500, mtry = 4) 

#View output and understand model
Model5.RanFor
#Check important variables
importance(Model5.RanFor,type = 2)
#Plot importance
varImpPlot(Model5.RanFor,type = 2, main = "Importance Plot")

#PART 4 UNDERSTANDING VARIABLE IMPACT - EFFECT PLOTS AND PARTIAL DEPENDENCE PLOT
#Let's understand more about the different predictor variables. You can do this with the logistic regression model using an effect plot
#Include the script for individual effect
plot(allEffects(Model2.LogReg),type = "response")

#Plot partial dependence plot for random forest to understand better
#Plot partial dependence plot - important to include as.data.frame as an actual data frame is needed
partialPlot(Model5.RanFor,
            pred.data = as.data.frame(filter(Retention2017, Sample == "Validation")),
            x.var = eopenrate,
            sub = "Validation Set", 
            which.class = "Y")

#PART 5 LIFT CHARTS AND ROC CURVES

#LIFT CHARTS
#View proportion of Y and N for lost to enter true response rate in lift chart
#Based on a study of the data 4749/30729 of the customers were considered to be lost. For now we can use a true response rate of 0.025973

#Cumulative Lift
lift.chart(modelList = c("Model1.RPart", "Model2.LogReg", "Model3.StepReg", "Model4.NeuNet","Model5.RanFor"),
           data = filter(Retention2017, Sample == "Estimation"),
           targLevel = "Y", 
           trueResp = 4749/(30729-7685),
           type = "cumulative", sub = "Estimation")

lift.chart(modelList = c("Model1.RPart"),
           data = filter(Retention2017, Sample == "Validation"),
           targLevel = "Y", 
           trueResp = 4749/(30729-7685),
           type = "cumulative", sub = "Validation")


#Incremental Lift Chart for the Best Model - Note random forest models cannot be plotted
lift.chart(modelList = c("Model5.RanFor"),
           data = as.data.frame(filter(Retention2017, Sample == "Validation")),
           targLevel = "Y",
           trueResp = 4749/(30729-7685),
           type = "incremental", sub = "Validation")

#PART 6 SCORE HOLDOUTS
#Output probability for Model1.RPart
Retention2017$lost.Model1.RPart <- rawProbScore(model = "Model1.RPart",
                                           data = Retention2017,
                                          targLevel = "Y")
#Create scoring submission and export to CSV 
Submission.Model1.RPart <- Retention2017[Retention2017$Sample == "Holdout",c("custid","lost.Model1.RPart")]
names(Submission.Model1.RPart) <- c("custid", "score")
write.csv(Submission.Model1.RPart,"Submission.Model1.RPart.csv")

#Output probability for Model2.LogReg
Retention2017$lost.Model2.LogReg <- rawProbScore(model = "Model2.LogReg",
                                           data = Retention2017,
                                           targLevel = "1")
#Create scoring submission and export to CSV
Submission.Model2.LogReg <- Retention2017[Retention2017$Sample == "Holdout",c("custid","lost.Model2.LogReg")]
names(Submission.Model2.LogReg) <- c("custid", "score")
write.csv(Submission.Model2.LogReg,"Submission.Model2.LogReg.csv")

#Output probability for Model3.StepReg
Retention2017$lost.Model3.StepReg <- rawProbScore(model = "Model3.StepReg",
                                                 data = Retention2017,
                                                 targLevel = "1")
#Create scoring submission and export to CSV
Submission.Model3.StepReg <- Retention2017[Retention2017$Sample == "Holdout",c("custid","lost.Model3.StepReg")]
names(Submission.Model3.StepReg) <- c("custid", "score")
write.csv(Submission.Model3.StepReg,"Submission.Model3.StepReg.csv")

#Output probability for Model4.NeuNet
Retention2017$lost.Model4.NeuNet <- rawProbScore(model = "Model4.NeuNet",
                                            data = Retention2017,
                                            targLevel = "1")
#Create scoring submission and export to CSV
Submission.Model4.NeuNet <- Retention2017[Retention2017$Sample == "Holdout",c("custid","lost.Model4.NeuNet")]
names(Submission.Model4.NeuNet) <- c("custid", "score")
write.csv(Submission.Model4.NeuNet,"Submission.Model4.NeuNet.csv")

#Output probability for Model5.RanFor
Retention2017$lost.Model5.RanFor <- predict(Model5.RanFor,
                                       Retention2017, 
                                       "prob")

#Create scoring submission and export to CSV
Submission.Model5.RanFor <- Retention2017[Retention2017$Sample == "Holdout",c("custid","lost.Model5.RanFor")]
names(Submission.Model5.RanFor) <- c("custid", "score")
write.csv(Submission.Model5.RanFor,"Submission.Model5.RanFor.csv")


