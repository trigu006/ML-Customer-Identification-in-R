# Title:  Identify prospects for car loans and assess their probability of acceptance
# Author: Trigueros, R.

options(scipen = 999)

# Libs
library(dplyr)
library(vtreat)
library(caret)
library(psych)
library(randomForest)
library(DescTools)
library(stringr)
library(expss)
library(ggpubr)
library(tidyr)
library(ggrepel)

# Function declarations
Top_N_Accuracy <- function(predictions, df, top_n = 100, cutoff = 0.5) {
  top_n_df <- cbind(df, predictions)
  top_n_df <- top_n_df[order(top_n_df$predictions, decreasing = TRUE),][1:top_n,]
  top_n_df$accuracy <- ifelse(ifelse(top_n_df$predictions > cutoff, 1, 0) == top_n_df$Y_AcceptedOffer,
                              1, 0)
  return(sum(top_n_df$accuracy/top_n))
  #return(paste('Accuracy: ', sum(top_n_df$accuracy/top_n) * 100, '%', sep = ''))
}

donutChart <- function(df) {
  df_mod  <- data.frame(category = c('No', 'Yes'),
                        accepted = c(table(df %>% select(Y_AcceptedOffer))[1],
                                     table(df %>% select(Y_AcceptedOffer))[2]))
  df_mod$fraction <-  df_mod$accepted/sum(df_mod$accepted)
  df_mod$ymax     <-  cumsum(df_mod$fraction)
  df_mod$ymin     <-  c(0, head(df_mod$ymax, n = 1))
  df_mod$label    <-  paste0(df_mod$category, ': ', round(df_mod$fraction * 100), '%')
  df_mod$position <-  (df_mod$ymax + df_mod$ymin) / 2
  return(
    ggplot(data = df_mod, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = ifelse(category == 'No', '#3db893', '#e6f5f0'))) +
      geom_rect() + geom_label(x = 3.5, aes(y = position, label = label), size = 5) + 
      coord_polar(theta = 'y') +
      xlim(c(2, 4)) + theme_void() + theme(legend.position = 'none')
  )
}

Acc_Graph <- function(df) {
  accept_df <- data.frame(hour = sort(unique(df$callStartHour)), acceptance = table(df$callStartHour, df$Y_AcceptedOffer)[,2]/table(df$callStartHour))
  return(ggplot(data = accept_df, aes(x = hour)) + geom_col(aes(y = acceptance.Freq), alpha = 0.5) +
           scale_x_continuous(breaks = seq(9, 17, 1)) + scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
           labs(y = '% Accepted', x = 'Hour'))
}

# Set up the working directory based on your files' location
setwd("")

# Raw data, need to add others
currentData   <- read.csv('CurrentCustomerMktgResults.csv')
vehicle_data  <- read.csv('householdVehicleData.csv')
credit_data   <- read.csv('householdCreditData.csv')
demog_data    <- read.csv('householdAxiomData.csv')

# Perform a join, need to add other data sets
joinData <- left_join(currentData, vehicle_data, by = c('HHuniqueID'))
joinData <- left_join(joinData, credit_data, by = c('HHuniqueID'))
joinData <- left_join(joinData, demog_data, by = c('HHuniqueID'))

# This is a classification problem so ensure R knows Y isn't 0/1 as integers
joinData$Y_AcceptedOffer  <-  as.factor(joinData$Y_AcceptedOffer)
joinData$Y_AcceptedOffer  <-  factor(joinData$Y_AcceptedOffer,
                                   levels = c('DidNotAccept', 'Accepted'),
                                   labels = c(0, 1))
joinData$annualDonations  <-  joinData$annualDonations %>% trimws() %>% str_replace(pattern = '\\$', '') %>%
                              str_replace(pattern = ',', '')
joinData$annualDonations  <-  ifelse(joinData$annualDonations != '', as.numeric(joinData$annualDonations), 0)
joinData$Donor            <-  ifelse(joinData$annualDonations > 0, 'Yes', 'No')
joinData$previousContact  <-  ifelse(joinData$PrevAttempts > 0, 'Yes', 'No')
joinData$callStartHour    <-  stringr::str_split(string = joinData$CallStart, pattern = '\\:', simplify = TRUE)[,1] %>% as.numeric()
joinData$callStartStamp   <-  stringr::str_split(string = joinData$CallStart, pattern = '\\:', simplify = TRUE)[,3] %>% as.numeric() +
                              stringr::str_split(string = joinData$CallStart, pattern = '\\:', simplify = TRUE)[,2] %>% as.numeric() * 60 +
                              stringr::str_split(string = joinData$CallStart, pattern = '\\:', simplify = TRUE)[,1] %>% as.numeric() * 3600
joinData$callEndStamp     <-  stringr::str_split(string = joinData$CallEnd, pattern = '\\:', simplify = TRUE)[,3] %>% as.numeric() +
                              stringr::str_split(string = joinData$CallEnd, pattern = '\\:', simplify = TRUE)[,2] %>% as.numeric() * 60 +
                              stringr::str_split(string = joinData$CallEnd, pattern = '\\:', simplify = TRUE)[,1] %>% as.numeric() * 3600
joinData$callDuration     <-  joinData$callEndStamp - joinData$callStartStamp
joinData$Communication    <-  ifelse(is.na(joinData$Communication), 'Not Available', joinData$Communication)

## SAMPLE: Partition schema
set.seed(1234)
idx         <- sample(nrow(joinData), 0.8 * nrow(joinData))
trainData   <- joinData[idx,]
validData   <- joinData[-idx,]

## EXPLORE: EDA, perform your EDA

  # Default on records
  cro(joinData$DefaultOnRecord, joinData$Y_AcceptedOffer)
  
  # INSIGHT #1: Single households with a tertiary education seem to be the only group with a higher probability of accepting a loan.
  cro(joinData$Education, joinData$Marital, joinData$Y_AcceptedOffer)
  ggplot(data = joinData, aes(x = Marital, fill = Y_AcceptedOffer)) +
    geom_bar(position = 'dodge') +
    facet_wrap(Education ~ .) +
    labs(y = 'Households') +
    scale_x_discrete('Education', labels = c('divorced' = 'Divorced',
                                              'married'  = 'Married',
                                              'single'   = 'Single'))
  ggpubr::ggarrange(
    donutChart(joinData %>% filter(Education == 'tertiary' & Marital == 'single' & PetsPurchases == TRUE)),
    donutChart(joinData %>% filter(Education == 'tertiary' & Marital == 'single' & PetsPurchases == FALSE)),
    ncol = 2
  )
  
  # INSIGHT #2: If you were previously contacted and do not possess household insurance, you are more likely to accept
  expss::cro(joinData$previousContact, joinData$Y_AcceptedOffer)
  data.frame(previousContact = c('No', 'Yes'),
             accepted        = c(table(joinData$previousContact, joinData$Y_AcceptedOffer)[3]/sum(table(joinData$previousContact, joinData$Y_AcceptedOffer)[c(1,3)]),
                                 table(joinData$previousContact, joinData$Y_AcceptedOffer)[4]/sum(table(joinData$previousContact, joinData$Y_AcceptedOffer)[c(2,4)]))) %>%
    ggplot(aes(x = previousContact, y = accepted)) + geom_col(width = 0.5, alpha = 0.5) + #theme_cleveland() +
    scale_y_continuous(label = scales::percent) + labs(x = 'Previously Contacted?', y = 'Accepted %') + geom_text(aes(label = paste(round(accepted * 100, 1), '%')))
  cro(joinData$previousContact, joinData$Y_AcceptedOffer, joinData$HHInsurance)
  ggpubr::ggarrange(
    donutChart(joinData %>% filter(previousContact == 'Yes' & HHInsurance == 0)),
    donutChart(joinData %>% filter(previousContact == 'Yes' & HHInsurance == 1)),
    donutChart(joinData %>% filter(previousContact == 'No' & HHInsurance == 0)),
    donutChart(joinData %>% filter(previousContact == 'No' & HHInsurance == 1)),
    ncol = 4
  )
    # Alternate visualizations
    data.frame(category = c('No HH | No Previous', 'No HH | Previous', 'HH | No Previous', 'HH | Previous'),
               accept   = c(table(joinData$previousContact, joinData$Y_AcceptedOffer, joinData$HHInsurance)[3]/sum(table(joinData$previousContact, joinData$Y_AcceptedOffer, joinData$HHInsurance)[c(1,3)]),
                            table(joinData$previousContact, joinData$Y_AcceptedOffer, joinData$HHInsurance)[4]/sum(table(joinData$previousContact, joinData$Y_AcceptedOffer, joinData$HHInsurance)[c(2,4)]),
                            table(joinData$previousContact, joinData$Y_AcceptedOffer, joinData$HHInsurance)[7]/sum(table(joinData$previousContact, joinData$Y_AcceptedOffer, joinData$HHInsurance)[c(5,7)]),
                            table(joinData$previousContact, joinData$Y_AcceptedOffer, joinData$HHInsurance)[8]/sum(table(joinData$previousContact, joinData$Y_AcceptedOffer, joinData$HHInsurance)[c(6,8)])
               )) %>% ggplot(aes(y = category, x = accept)) + geom_col(alpha = 0.5, width = 0.5) +
      scale_x_continuous(label = scales::percent) + geom_text(aes(label = paste(round(accept * 100, 1), '%'))) +
      labs(x = 'Accepted Loan?', y = '')
  
  # INSIGHT #3: Retirees over-index in acceptance, especially for late mornings (10 a.m. - 12 p.m.) with both cell phones and telephones
  retired <- joinData %>% filter(Job == 'retired')
  table(retired$Y_AcceptedOffer, retired$Communication)
  donutChart(retired)
  comm_df   <- data.frame(communication = sort(unique(retired$Communication)),
                          acceptance = table(retired$Communication, retired$Y_AcceptedOffer)[,2]/table(retired$Communication))
  comm_df %>% mutate(communication = reorder(communication, acceptance.Freq)) %>% ggplot(aes(x = acceptance.Freq, y = communication)) + geom_col(alpha = 0.3, width = 0.5) + labs(x = '% Accepted') +
    scale_x_continuous(label = scales::percent) + geom_text(aes(label = paste(round(acceptance.Freq * 100, 1), '%'))) +
    labs(y = '')
  # Gender analysis among retirees
  ggpubr::ggarrange(donutChart(joinData %>% filter(Job == 'retired' & headOfhouseholdGender == 'F')),
                    donutChart(joinData %>% filter(Job == 'retired' & headOfhouseholdGender == 'M')), nrow = 2)
  # Acceptance %: Retirees vs. male retirees
  ggpubr::ggarrange(Acc_Graph(joinData %>% filter(Job == 'retired')) + labs(x = 'Retirees'),
                    Acc_Graph(joinData %>% filter(Job == 'retired' & headOfhouseholdGender == 'M')) + labs(x = 'Male Retirees')
                    , nrow = 2)
  # Acceptance %: Telephone vs. overall
  ggpubr::ggarrange(Acc_Graph(joinData %>% filter(Job == 'retired' & Communication == 'telephone')) + labs(x = 'Telephone'),
                    Acc_Graph(joinData %>% filter(Job == 'retired')) + labs(x = 'Overall')
                    , nrow = 2)
  Acc_Graph(joinData %>% filter(Job == 'retired')) + coord_cartesian(ylim = c(0, 0.85))
  Acc_Graph(joinData %>% filter(Job == 'retired' & Communication == 'telephone')) + coord_cartesian(ylim = c(0, 0.85))  
    
  # INSIGHT #4: Student-job group with (A) age range of 18-25 and (B) affluent purchases tend to over-index in accepting the marketing offer
  donutChart(joinData %>% filter(Job == 'student'))
  cro(joinData$Y_AcceptedOffer[joinData$Job == 'student'], joinData$Age[joinData$Job == 'student'])
  students <- joinData %>% filter(Job == 'student')
  ggpubr::ggarrange(
    data.frame(age = sort(unique(students$Age)), accepted = table(students$Age, students$Y_AcceptedOffer)[,2]/table(students$Age), absolute = table(students$Age, students$Y_AcceptedOffer)[,2]) %>%
      ggplot() + geom_col(aes(x = age, y = accepted.Freq), alpha = 0.5) + scale_y_continuous(label = scales::percent),
    data.frame(age = sort(unique(students$Age)), absolute = table(students$Age, students$Y_AcceptedOffer)[,2]) %>%
      ggplot() + geom_col(aes(x = age, y = absolute), alpha = 0.5),
    nrow = 2)
  students_df <- data.frame(age = sort(unique(students$Age)), accepted = table(students$Age, students$Y_AcceptedOffer)[,2]/table(students$Age), absolute = table(students$Age, students$Y_AcceptedOffer)[,2])
  # Over 60 percent of accepted offers lie in a seven-year age range (18 - 25, youngest)
  sum(students_df$absolute[students_df$age <= 25])/sum(students_df$absolute)
  student_subset <- subset(joinData, Job == 'student' & Age <= 25)
  cro(student_subset$Y_AcceptedOffer, student_subset$AffluencePurchases)
  data.frame(accepted = c('No', 'Yes'), percentage = c(table(student_subset$Y_AcceptedOffer, student_subset$AffluencePurchases)[1,2]/table(student_subset$Y_AcceptedOffer)[1],
                                                       table(student_subset$Y_AcceptedOffer, student_subset$AffluencePurchases)[2,2]/table(student_subset$Y_AcceptedOffer)[2])) %>%
    ggplot(aes(x = percentage, y = accepted)) + geom_col(alpha = 0.5, width = 0.5) +
    scale_x_continuous(label = scales::percent) + geom_text(aes(label = paste(round(percentage * 100, 1), '%'))) +
    labs(x = '% Affluent Purchases', y = 'Accepted')
  
  # OTHER EDA: Not present in presentation
  # NOTE: Please skip to Row 236 to continue presentation
    # Contact analysis
      # Duration by communication: cell phones appear to have (A) longer durations and (B) bigger deviations
      ggplot(data = joinData, aes(x = ifelse(Y_AcceptedOffer == 1, 'Yes', 'No'), y = callDuration)) +
        geom_boxplot() + coord_cartesian(ylim = c(0, 1000)) +
        labs(x = 'Accepted Offer?', y = 'Call Duration (Seconds)') +
        facet_wrap(~Communication, ncol = 3)
      # Accepted % by time of contact
      Acc_Graph(joinData)
      table(joinData$callStartHour, joinData$Y_AcceptedOffer)
      # Periods of last contacts
      cro(joinData$LastContactMonth, joinData$Y_AcceptedOffer)
      cro(joinData$LastContactDay, joinData$Y_AcceptedOffer)
    
    # Donors
      # Donors: Age by Donations
      ggplot(data = joinData, aes(x = annualDonations, y = Age, color = Y_AcceptedOffer)) +
        geom_point()
      # Donors (Boolean) and acceptance of offers
      table(joinData$Donor, joinData$Y_AcceptedOffer)
      cro(joinData$CarLoan, joinData$Y_AcceptedOffer, joinData$HHInsurance)
    
    # Previous history
    table(joinData$Y_AcceptedOffer, joinData$past_Outcome)  
    ggplot(joinData, aes(x = past_Outcome, fill = past_Outcome)) +
      geom_bar() +
      facet_wrap(~Y_AcceptedOffer, ncol = 2)
    
    # Correlation analysis: selected variables
    cor_variables <- c('Age', 'RecentBalance', 'DefaultOnRecord', 'NoOfContacts', 'annualDonations')
    cor(joinData[cor_variables])
    
    # Recent balance: overall and breakdown by gender
    ggplot(data = joinData, aes(x = Age, y = RecentBalance, color = Y_AcceptedOffer)) +
      geom_point() +
      facet_wrap(. ~ headOfhouseholdGender, nrow = 2)
    ggplot(data = joinData, aes(y = RecentBalance, x = callDuration, color = Y_AcceptedOffer)) + geom_point()
  
    # Channel of communication
    cro(joinData$Communication, joinData$Y_AcceptedOffer, joinData$AffluencePurchases)
  
    # Affluent purchases
    table(joinData$AffluencePurchases, joinData$Y_AcceptedOffer)
    cro(joinData$AffluencePurchases, joinData$Y_AcceptedOffer, joinData$PetsPurchases)
  
    # Web behavior
    cro(joinData$Y_AcceptedOffer, joinData$DigitalHabits_5_AlwaysOn, joinData$headOfhouseholdGender)
    cro(joinData$Y_AcceptedOffer, joinData$Marital, joinData$headOfhouseholdGender)
    cro(joinData$Y_AcceptedOffer, joinData$PetsPurchases, joinData$headOfhouseholdGender)
  
    # Job breakdown
    cro(joinData$Y_AcceptedOffer, joinData$Job)
  
## MODIFY: Vtreat, need to declare xVars & name of Y var
xVars <- c(# Main
          'DaysPassed', 'Communication', 'past_Outcome',
          'LastContactDay', 
          'LastContactMonth', 'NoOfContacts',
  # Vehicle
          'carYr',
  # Demographic
           'headOfhouseholdGender',
          # Removed variables: 'annualDonation', 'Donor', 'carModel', 'carMake', 'EstRace', 'callDuration', 'carMake'
          # NOTES: Model improves markedly without 'carModel' due to many values with few observations
            'PetsPurchases', 'DigitalHabits_5_AlwaysOn',
           'AffluencePurchases', 'Age', 'Job', 'Marital', 'Education',
  # Credit data
          'DefaultOnRecord',
          'RecentBalance', 'HHInsurance', 'CarLoan',
          # Added variables
          'previousContact')

yVar  <- 'Y_AcceptedOffer'
plan  <- designTreatmentsC(trainData, xVars, yVar, 1)

# Apply the rules to the set
treatedTrain <- prepare(plan, trainData)
treatedValid  <- prepare(plan, validData)

## MODEL: caret etc.
# MODEL 1: Random forest
#weights <- ifelse(treatedTrain$Y_AcceptedOffer == 1, (1/table(treatedTrain$Y_AcceptedOffer[1])) * 0.5, (1/table(treatedTrain$Y_AcceptedOffer[2])) * 0.5)
model_weights <- ifelse(treatedTrain$Y_AcceptedOffer == 1,
                    1 - table(treatedTrain$Y_AcceptedOffer)[2]/sum(table(treatedTrain$Y_AcceptedOffer)),
                    1 - table(treatedTrain$Y_AcceptedOffer)[1]/sum(table(treatedTrain$Y_AcceptedOffer)))

  # Sensitivity analysis of number of features
  mtry_df <- data.frame(features = NA, train_accuracy = NA, valid_accuracy = NA)
  
  for (i in seq(from = 2, to = 10, by = 1)) {
    set.seed(1234)
    forest            <- randomForest(data = treatedTrain, Y_AcceptedOffer ~ . - DefaultOnRecord - Education_lev_NA - past_Outcome_lev_NA,
                                      importance = TRUE, ntree = 300, mtry = i)
    mtry_train_preds <- predict(forest, treatedTrain)
    mtry_valid_preds <- predict(forest, treatedValid)
    row <- c(i,
             caret::confusionMatrix(mtry_train_preds, treatedTrain$Y_AcceptedOffer)$overall[1],
             caret::confusionMatrix(mtry_valid_preds, treatedValid$Y_AcceptedOffer)$overall[1])
    mtry_df          <- rbind(mtry_df, row)
  }
  mtry_df
  mtry_int <- mtry_df %>% filter(!is.na(valid_accuracy)) %>% filter(valid_accuracy == max(valid_accuracy)) %>% select(features) %>% as.numeric()

set.seed(1234)
rf  <- randomForest(data = treatedTrain,
                    Y_AcceptedOffer ~ . - DefaultOnRecord - Education_lev_NA - past_Outcome_lev_NA,
                    importance = TRUE,
                    weights = model_weights,
                    mtry = mtry_int
                    #,ntree = 150
                    )

importance <- importance(rf, type = 2)
importance[order(importance[,1], decreasing = TRUE),]

plot(rf)
print(rf)

  ## ASSESS: Predict & calculate the KPI appropriate for classification
  trainingPreds <- predict(rf, treatedTrain, type = 'class')
  testingPreds  <- predict(rf, treatedValid, type = 'class')
  caret::confusionMatrix(trainingPreds, treatedTrain$Y_AcceptedOffer)
  caret::confusionMatrix(testingPreds, treatedValid$Y_AcceptedOffer)
  
  # Calculate probabilistic predictions to assess top N range
  trainingPredsProb <- predict(rf, treatedTrain, type = 'prob')[,2]
  testingPredsProb  <- predict(rf, treatedValid, type = 'prob')[,2]
  
  # Top N accuracy analysis (i.e., accuracy of highest probabilities)
  # NOTE: To make the percentages consistent (i.e., top 10% of prospects [100 households]), we will
  # select the top 10% of the sample size of the training and validation sets for accuracy.
  Top_N_Accuracy(predictions = trainingPredsProb, treatedTrain, top_n = 320)
  # Validation set over-indexes in accuracy for top observations
  Top_N_Accuracy(predictions = testingPredsProb, treatedValid, top_n = 80)
  
  randomForest::varImpPlot(rf)

# MODEL 2: Logistic regression
set.seed(1234)
logit_model <- glm(data = treatedTrain,
                   Y_AcceptedOffer ~ . - DefaultOnRecord - Education_lev_NA - past_Outcome_lev_NA,
                   family = 'binomial')

  # Predictions
  logit_train_preds <- predict(logit_model, treatedTrain, type = 'response')
  logit_valid_preds <- predict(logit_model, treatedValid, type = 'response')
  
  # Top N accuracy analysis
  (logit_train_accuracy <- Top_N_Accuracy(predictions = logit_train_preds, df = treatedTrain, top_n = 320))
  (logit_valid_accuracy <- Top_N_Accuracy(predictions = logit_valid_preds, df = treatedValid, top_n = 80))
  
  # Select cutoff
  logit_cutoff_df   <- data.frame(cutoff = NA, train_accuracy = NA, valid_accuracy = NA)
  for (i in seq(from = 0.4, to = 0.6, by = 0.01)) {
    row <- c(i,
             confusionMatrix(as.factor(ifelse(logit_train_preds > i, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer))$overall[1],
             confusionMatrix(as.factor(ifelse(logit_valid_preds > i, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer))$overall[1]
             )
    logit_cutoff_df <- rbind(logit_cutoff_df, row)
  }
  logit_cutoff_df
  selected_cutoff <- logit_cutoff_df$cutoff[which(logit_cutoff_df$valid_accuracy == max(logit_cutoff_df$valid_accuracy, na.rm = TRUE))[1]]
  
  # Assess accuracy
  caret::confusionMatrix(as.factor(ifelse(logit_train_preds > selected_cutoff, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer))
  caret::confusionMatrix(as.factor(ifelse(logit_valid_preds > selected_cutoff, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer))

# MODEL 3: Neural networks
  
  # Create network with three hidden (interim) layers
  set.seed(1234)
  nn  <- caret::train(as.factor(Y_AcceptedOffer) ~ . - DefaultOnRecord - Education_lev_NA - past_Outcome_lev_NA,
                      data = treatedTrain,
                      method = 'nnet',
                      weights = model_weights)
  plot(nn)
  
  # Predict
  nn_train_preds <- predict(nn, treatedTrain, type = 'prob')
  nn_valid_preds <- predict(nn, treatedValid, type = 'prob')
  
  nn_cutoff_df  <- data.frame(cutoff = NA, train_accuracy = NA, valid_accuracy = NA)
  
  # Select cutoff
  for (i in seq(from = 0.4, to = 0.95, by = 0.01)) {
    row <- c(i,
             confusionMatrix(as.factor(ifelse(nn_train_preds[,2] > i, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer))$overall[1],
             confusionMatrix(as.factor(ifelse(nn_valid_preds[,2] > i, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer))$overall[1]
    )
    nn_cutoff_df <- rbind(nn_cutoff_df, row)
  }
  nn_cutoff_df
  nn_cutoff <- nn_cutoff_df$cutoff[which(nn_cutoff_df$valid_accuracy == max(nn_cutoff_df$valid_accuracy, na.rm = TRUE))[1]]
  
  # Assess accuracy
  confusionMatrix(as.factor(ifelse(nn_train_preds[,2] > nn_cutoff, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer), positive = '1')
  confusionMatrix(as.factor(ifelse(nn_valid_preds[,2] > nn_cutoff, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer), positive = '1')
  
  # Top N
  (Top_N_Accuracy(predictions = nn_train_preds[,2], df = treatedTrain, top_n = 320, cutoff = nn_cutoff))
  (Top_N_Accuracy(predictions = nn_valid_preds[,2], df = treatedValid, top_n = 80, cutoff = nn_cutoff))
  
# Model 4: k-NN
  knnTreatedTrain <- treatedTrain %>% mutate(Y_AcceptedOffer = as.integer(Y_AcceptedOffer)) %>% mutate(Y_AcceptedOffer = ifelse(Y_AcceptedOffer == 1, 0, 1))
  knnTreatedValid <- treatedValid %>% mutate(Y_AcceptedOffer = as.integer(Y_AcceptedOffer)) %>% mutate(Y_AcceptedOffer = ifelse(Y_AcceptedOffer == 1, 0, 1))
  
  # Select 'k' based on the accuracy of the validation set
  knn_df <- data.frame(k = NA, train_accuracy = NA, valid_accuracy = NA, top10_train = NA, top10_valid = NA)
  for (i in seq(from = 5, to = 40, by = 1)) {
    set.seed(1234)
    knn_k  <- caret::train(as.factor(Y_AcceptedOffer) ~ . - DefaultOnRecord - Education_lev_NA - past_Outcome_lev_NA,
                           data = knnTreatedTrain, method = 'knn', preProcess = c('center', 'scale'), tuneGrid = data.frame(k = i))
    k_train_accuracy <- confusionMatrix(predict(knn_k, knnTreatedTrain), as.factor(knnTreatedTrain$Y_AcceptedOffer))$overall[1]
    k_valid_accuracy <- confusionMatrix(predict(knn_k, knnTreatedValid), as.factor(knnTreatedValid$Y_AcceptedOffer))$overall[1]
    top10_train_acc  <- Top_N_Accuracy(predictions = predict(knn_k, knnTreatedTrain, type = 'prob')[,2], df = knnTreatedTrain, top_n = 320)
    top10_valid_acc  <- Top_N_Accuracy(predictions = predict(knn_k, knnTreatedValid, type = 'prob')[,2], df = knnTreatedValid, top_n = 80)
    row    <- c(i, k_train_accuracy, k_valid_accuracy, top10_train_acc, top10_valid_acc)
    knn_df <- rbind(knn_df, row)
  }
  knn_df %>% filter(!is.na(k))
  selected_k <- knn_df$k[which(knn_df$valid_accuracy == max(knn_df$valid_accuracy, na.rm = TRUE))[1]]
  
  set.seed(1234)
  knn <- caret::train(#as.factor(Y_AcceptedOffer) ~ .,
                      as.factor(Y_AcceptedOffer) ~ . - DefaultOnRecord - Education_lev_NA - past_Outcome_lev_NA,
                      data = knnTreatedTrain,
                      method = 'knn',
                      preProcess = c('center', 'scale'),
                      weights = model_weights,
                      tuneGrid = data.frame(k = selected_k)
                      #tuneLength = 20
                      )
  
  # Run predictions
  knn_train_preds   <- predict(knn, knnTreatedTrain)
  knn_valid_preds   <- predict(knn, knnTreatedValid)
  
  # Assess accuracy
  confusionMatrix(knn_train_preds, as.factor(knnTreatedTrain$Y_AcceptedOffer))
  confusionMatrix(knn_valid_preds, as.factor(knnTreatedValid$Y_AcceptedOffer))
  
  # Top N analysis
  knn_train_preds_probs   <- predict(knn, knnTreatedTrain, type = 'prob')[,2]
  knn_valid_preds_probs   <- predict(knn, knnTreatedValid, type = 'prob')[,2]
  (Top_N_Accuracy(predictions = knn_train_preds_probs, df = knnTreatedTrain, top_n = 320))
  (Top_N_Accuracy(predictions = knn_valid_preds_probs, df = knnTreatedValid, top_n = 80))
  
# Composite data frame
  composite_df <- data.frame(model = 'Random Forest', set = 'Training', accuracy = caret::confusionMatrix(trainingPreds, treatedTrain$Y_AcceptedOffer)$overall[1], top10_accuracy = Top_N_Accuracy(predictions = trainingPredsProb, treatedTrain, top_n = 320), sensitivity = caret::confusionMatrix(trainingPreds, treatedTrain$Y_AcceptedOffer, positive = '1')$byClass[1], kappa = caret::confusionMatrix(trainingPreds, treatedTrain$Y_AcceptedOffer)$overall[2])
  composite_df <- rbind(composite_df,
                        c('Random Forest', 'Validation', caret::confusionMatrix(testingPreds, treatedValid$Y_AcceptedOffer)$overall[1], Top_N_Accuracy(predictions = testingPredsProb, treatedValid, top_n = 80), caret::confusionMatrix(testingPreds, treatedValid$Y_AcceptedOffer, positive = '1')$byClass[1], caret::confusionMatrix(testingPreds, treatedValid$Y_AcceptedOffer)$overall[2]),
                        # Logistic regression
                        c('Logistic Regression', 'Training', caret::confusionMatrix(as.factor(ifelse(logit_train_preds > selected_cutoff, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer))$overall[1], logit_train_accuracy,caret::confusionMatrix(as.factor(ifelse(logit_train_preds > selected_cutoff, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer), positive = '1')$byClass[1], caret::confusionMatrix(as.factor(ifelse(logit_train_preds > selected_cutoff, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer))$overall[2]),
                        c('Logistic Regression', 'Validation', caret::confusionMatrix(as.factor(ifelse(logit_valid_preds > selected_cutoff, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer))$overall[1], logit_valid_accuracy, caret::confusionMatrix(as.factor(ifelse(logit_valid_preds > selected_cutoff, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer), positive = '1')$byClass[1], caret::confusionMatrix(as.factor(ifelse(logit_valid_preds > selected_cutoff, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer))$overall[2]),
                        # k-NN
                        c('k-NN', 'Training', confusionMatrix(knn_train_preds, as.factor(knnTreatedTrain$Y_AcceptedOffer))$overall[1], Top_N_Accuracy(predictions = knn_train_preds_probs, df = knnTreatedTrain, top_n = 320), confusionMatrix(knn_train_preds, as.factor(knnTreatedTrain$Y_AcceptedOffer), positive = '1')$byClass[1], confusionMatrix(knn_train_preds, as.factor(knnTreatedTrain$Y_AcceptedOffer))$overall[2]),
                        c('k-NN', 'Validation', confusionMatrix(knn_valid_preds, as.factor(knnTreatedValid$Y_AcceptedOffer))$overall[1], Top_N_Accuracy(predictions = knn_valid_preds_probs, df = knnTreatedValid, top_n = 80), confusionMatrix(knn_valid_preds, as.factor(knnTreatedValid$Y_AcceptedOffer), positive = '1')$byClass[1], confusionMatrix(knn_valid_preds, as.factor(knnTreatedValid$Y_AcceptedOffer))$overall[2]),
                        # Neural network
                        c('Neural Network', 'Training', confusionMatrix(as.factor(ifelse(nn_train_preds[,2] > nn_cutoff, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer))$overall[1], Top_N_Accuracy(predictions = nn_train_preds[,2], df = treatedTrain, top_n = 320, cutoff = nn_cutoff), confusionMatrix(as.factor(ifelse(nn_train_preds[,2] > nn_cutoff, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer), positive = '1')$byClass[1], confusionMatrix(as.factor(ifelse(nn_train_preds[,2] > nn_cutoff, 1, 0)), as.factor(treatedTrain$Y_AcceptedOffer))$overall[2]),
                        c('Neural Network', 'Validation', confusionMatrix(as.factor(ifelse(nn_valid_preds[,2] > nn_cutoff, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer))$overall[1], Top_N_Accuracy(predictions = nn_valid_preds[,2], df = treatedValid, top_n = 80, cutoff = nn_cutoff), confusionMatrix(as.factor(ifelse(nn_valid_preds[,2] > nn_cutoff, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer), positive = '1')$byClass[1], confusionMatrix(as.factor(ifelse(nn_valid_preds[,2] > nn_cutoff, 1, 0)), as.factor(treatedValid$Y_AcceptedOffer))$overall[2])
  )
  composite_df$accuracy       <- as.numeric(composite_df$accuracy)
  composite_df$top10_accuracy <- as.numeric(composite_df$top10_accuracy)
  composite_df$sensitivity    <- as.numeric(composite_df$sensitivity)
  composite_df$kappa          <- as.numeric(composite_df$kappa)
  composite_df$set            <- as.factor(composite_df$set)
  
  # SCATTER PLOT: Top-decile accuracy by overall accuracy
  composite_df %>% ggplot(aes(x = accuracy, y = top10_accuracy, shape = set, size = 2, color = model)) + geom_point() +
    scale_y_continuous(label = scales::percent, breaks = seq(from = 0.80, to = 1, by = 0.025)) + scale_x_continuous(label = scales::percent, breaks = seq(from = 0.7, to = 1, by = 0.05)) +
    ggrepel::geom_text_repel(aes(label = factor(model, levels = c('k-NN', 'Neural Network', 'Logistic Regression', 'Random Forest'), labels = c('k-NN', 'Neural', 'Logistic', 'RF') ))) +
    labs(x = 'Overall Accuracy', y = 'Top-Decile Accuracy') + theme(legend.position = c(0.88, 0.20)) + guides(size = FALSE)
  
  # Overall sensitivity
  composite_df %>% ggplot(aes(x = model, y = sensitivity, fill = set)) + geom_col(position = 'dodge', width = 0.5, alpha = 0.5) +
    scale_y_continuous(labels = scales::percent, breaks = seq(from = 0.0, to = 1, by = 0.1)) + labs(x = 'Model', y = 'Sensitivity (Overall)') + geom_text(aes(label = paste(round(as.numeric(sensitivity) * 100, 1), '%'))) + coord_flip() + coord_cartesian(ylim = c(0.3, 1)) +
    scale_x_discrete(breaks = c('k-NN', 'Logistic Regression', 'Neural Network', 'Random Forest'),
                     labels = c('k-NN', 'Logistic\nRegression', 'Neural\nNetwork', 'Random\nForest')) + theme(legend.position = c(0.15, 0.80))
  
  # Kappa accuracy
  composite_df %>% ggplot(aes(x = model, y = kappa, fill = set)) + geom_col(position = 'dodge', width = 0.5, alpha = 0.5) +
    scale_y_continuous(labels = scales::percent, breaks = seq(from = 0.0, to = 1, by = 0.1)) + labs(x = 'Model', y = 'Kappa Accuracy') + geom_text(aes(label = paste(round(as.numeric(kappa) * 100, 1), '%'))) + coord_flip() + coord_cartesian(ylim = c(0.3, 1)) +
    scale_x_discrete(breaks = c('k-NN', 'Logistic Regression', 'Neural Network', 'Random Forest'),
                     labels = c('k-NN', 'Logistic\nRegression', 'Neural\nNetwork', 'Random\nForest')) + theme(legend.position = c(0.15, 0.80))
  
## NOW TO GET PROSPECTIVE CUSTOMER RESULTS
# 1. Load Raw Data
prospects <- read.csv('../ProspectiveCustomers.csv')

# 2. Join with external data
joinProspects <- left_join(prospects, vehicle_data, by = c('HHuniqueID'))
joinProspects <- left_join(joinProspects, credit_data, by = c('HHuniqueID'))
joinProspects <- left_join(joinProspects, demog_data, by = c('HHuniqueID'))
  
  # Add factored variables
  joinProspects$previousContact <- ifelse(joinProspects$PrevAttempts > 0, 'Yes', 'No')

# 3. Apply a treatment plan
treatedProspects  <- prepare(plan, joinProspects)

# 4. Make predictions
nnProspectPreds     <- predict(nn, treatedProspects, type = 'prob')

# 5. Join probabilities back to ID
nnProspectResults   <- cbind(HHuniqueID = prospects$HHuniqueID, probability = as.numeric(nnProspectPreds[, 2]))

# 6. Identify the top 100 "success" class probabilities from prospectsResults
(top_100_nn         <- nnProspectResults[order(nnProspectResults[, 2], decreasing = TRUE),][1:100,])

################################################################################
# EDA on top-100 prospects
nn_prospects_df     <- inner_join(joinProspects, as.data.frame(top_100_nn), by = c('HHuniqueID'))

write.csv(nn_prospects_df, "/Users/trigu006/Documents/Courses/CSCI E-96 - Data Mining for Business/Case III - Submission/trigueros_Case_III_scores.csv", row.names = FALSE)

  # Gender - roughly equal split between females and males
  cro(nn_prospects_df$headOfhouseholdGender)
  gender_df       <- data.frame(gender = c('Female', 'Male'),
                        accepted =  c(table(nn_prospects_df %>% select(headOfhouseholdGender))[1],
                                      table(nn_prospects_df %>% select(headOfhouseholdGender))[2]))
  gender_df$fraction <-  gender_df$accepted/sum(gender_df$accepted)
  gender_df$ymax     <-  cumsum(gender_df$fraction)
  gender_df$ymin     <-  c(0, head(gender_df$ymax, n = 1))
  gender_df$label    <-  paste0(gender_df$gender, ': ', round(gender_df$fraction * 100), '%')
  gender_df$position <-  (gender_df$ymax + gender_df$ymin) / 2
  ggplot(data = gender_df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = ifelse(gender == 'Female', '#FFFFFF', '#e6f5f0'))) +
    geom_rect() + geom_label(x = 3.5, aes(y = position, label = label), size = 5) + 
    coord_polar(theta = 'y') +
    xlim(c(2, 4)) + theme_void() + theme(legend.position = 'none')

  # Household insurance and previous contact
  ggplot(data = nn_prospects_df, aes(x = ifelse(as.factor(HHInsurance) == '1', 'HH Ins.', 'No HH Ins.'), fill = ifelse(as.factor(HHInsurance) == '1', 'HH Ins.', 'No HH Ins.'))) + geom_bar(width = 0.5, alpha = 0.5) +
    facet_grid(. ~ previousContact) + labs(x = 'Previous Contact', y = 'Households') + theme(legend.position = 'none') +
    scale_y_continuous(breaks = seq(0, 60, 10))

  # Education and marital status
  cro(nn_prospects_df$Education, nn_prospects_df$Marital)
  ggplot(data = nn_prospects_df, aes(x = Marital, fill = Marital)) +
    geom_bar(position = 'dodge', alpha = 0.5) + facet_wrap(Education ~ .) +
    labs(y = 'Households') + scale_y_continuous(breaks = seq(0, 25, 5))
    scale_x_discrete('Education', labels = c('divorced' = 'Divorced',
                                             'married'  = 'Married',
                                             'single'   = 'Single'))
    
    