#username : mzr3
library(ggplot2)

# 0. Data Preprocessing ---------------------------------------------------

# Datasets readings
train <- read.csv('mnist_train.csv',
                  header = FALSE, sep = ',', stringsAsFactors = FALSE)
test <- read.csv('mnist_test.csv', 
                 header = FALSE,sep = ',', stringsAsFactors = FALSE)

#separate the datasets between 0_1 and 3_5
train_0_1 = train[which(train[785,] %in% c(1,0))]
train_3_5 = train[which(train[785,] %in% c(3,5))]
test_0_1 = test[which(test[785,] %in% c(1,0))]
test_3_5 = test[which(test[785,] %in% c(3,5))]

#separate the labels on another vector and the dataset 
true_label_train_0_1 = train_0_1[785,]
train_0_1 = train_0_1[1:784,]
true_label_train_3_5 = train_3_5[785,]
train_3_5 = train_3_5[1:784,]
true_label_test_0_1 = test_0_1[785,]
test_0_1 = test_0_1[1:784,]
true_label_test_3_5 = test_3_5[785,]
test_3_5 = test_3_5[1:784,]

#Transformation of the subsets in order to print the images
train_0_1_2D = matrix(data = train_0_1[,1], nrow = 28, ncol = 28)
train_0_1_2D = apply(train_0_1_2D,2,rev)
train_3_5_2D = matrix(data = train_3_5[,1], nrow = 28, ncol = 28)
train_3_5_2D = apply(train_3_5_2D,2,rev)
test_0_1_2D = matrix(data = test_0_1[,1500], nrow = 28, ncol = 28)
test_0_1_2D = apply(test_0_1_2D,2,rev)
test_3_5_2D = matrix(data = test_3_5[,1901], nrow = 28, ncol = 28)
test_3_5_2D = apply(test_3_5_2D,2,rev)

# Shox the figures with their labels
image(t(train_0_1_2D), col = gray.colors(256)) 
title(main = list(true_label_train_0_1[,1], cex = 1.5,
                  col = "red", font = 3))
image(t(train_3_5_2D), col = gray.colors(256))
title(main = list(true_label_train_3_5[,1], cex = 1.5,
                  col = "red", font = 3))
image(t(test_0_1_2D), col = gray.colors(256))
title(main = list(true_label_test_0_1[,1500], cex = 1.5,
                  col = "red", font = 3))
image(t(test_3_5_2D), col = gray.colors(256))
title(main = list(true_label_test_3_5[,1901], cex = 1.5,
                  col = "red", font = 3))


# Implementation  ------------------------------------------------------------------

# Preparing input values

data_train_0_1 = as.matrix(t(train_0_1))
true_label_train_0_1 = as.vector(t(true_label_train_0_1))
data_train_3_5 = as.matrix(t(train_3_5))
true_label_train_3_5 = as.vector(t(true_label_train_3_5))
data_test_0_1 = as.matrix(t(test_0_1))
true_label_test_0_1 = as.vector(t(true_label_test_0_1))
data_test_3_5 = as.matrix(t(test_3_5))
true_label_test_3_5 = as.vector(t(true_label_test_3_5))

# Sigmoid Function

sigmoid_function <- function(x)
  {
  return(1/(1+exp(-x)))
}

# Cost function
#compute the derivative of the loss function for one iteration
cost_function <- function(theta,X,Y) # computes cost given predictal and actual values
{
  n = dim(X)[1] # number of training examples
  
  gradient = t((1/n)*(t(X) %*% ((sigmoid_function(X %*% t(theta))) - Y)))

  return(gradient)
}

# Gradient Descent function with two choices of convergence criteria
gradient_descent <- function(X,Y, conv_criterion,conv_criterion2,max_iterations, learning_rate, theta_ini,type){
  X = cbind(X,rep(1,dim(X)[1])) #add a line of ones for the c in the sum of theta_i*x_i
  m  = dim(X)[1]
  theta = theta_ini
  i = 0
  d = 1
  J = 1
  J1= 0
  # First convergence criteria on difference on theta
  if (type == 1){
    while (norm(d,type = "2") >= conv_criterion && i <= max_iterations){
      #compute the error
      d = learning_rate*cost_function(theta,X,Y)
      #compute theta
      theta = theta - d
      i = i+1
      # Decrease the learning rate
      learning_rate = learning_rate * 0.9
      if (i == max_iterations){
        cat("Problem Convergence and the error is", norm(d,type = "2"),end="\n", file="")
      }
    }
    cat("The number of iterations is", i,end="\n", file="")
  }
  # Second convergence criteria on difference of loss functions
  if (type == 2) {
    while (abs(J-J1) >= conv_criterion2 && i <= max_iterations){
      #Compute the error
      d = learning_rate*cost_function(theta,X,Y)
      #Compute the theta
      theta = theta - d
      h = sigmoid_function(X %*% as.vector(theta))
      i = i+1
      J1 = J
      # Compute the loss function
      J <- (1/m)*sum((-Y*log(h)) - ((1-Y)*log(1-h)))
      if (i %% 100 == 0){
        cat("The error is", abs(J-J1) ,'and i = ', i,end="\n", file="")
      }   
      # Decrease the learning rate
      learning_rate = learning_rate * 0.9
      if (i == max_iterations){
        cat("Problem Convergence and the error is", abs(J-J1),end="\n", file="")
      }
    }
    cat("The number of iterations is", i,end="\n", file="")
  }
  return(theta)
}

# Question 3 ----------------------------------------------------------------

# Parameters
theta_initialization <- function(X){
  theta = matrix(0, 1, dim(X)[2]+1)
  return(theta)
}

alpha = 1 #learning rate initial
Conv.Criterion = 1E-3 #threshold for the first convergence criteria
Conv.Criterion2 = 1E-3 #threshold for the second convergence criteria
number_iterations = 3000 #number maximum of iterations


#this function allows to classify a dataset with a theta computed before during the training phasis
accuracy <- function(dataframe,output,theta){
  #adjust the size of the dataframe by adding a line of ones for the c
  dataframe = cbind(dataframe,rep(1,dim(dataframe)[1]))
  
  #compute the sigmoid function that gives the probabilities to be 0 or 1
  h = sigmoid_function(dataframe %*% as.vector(theta))
  
  # Assign the labels according to the probabilities
  h[which(h < 0.5)] = 0
  h[which(h >= 0.5)] = 1 
  
  # Compare the found label to the reals ones
  test = which(h != output)
  return(((dim(dataframe)[1]-length(test))/dim(dataframe)[1])*100)
}

# Training train_0_1 

# Compute theta
theta_train_0_1= gradient_descent(data_train_0_1,true_label_train_0_1,Conv.Criterion,Conv.Criterion2,number_iterations, alpha,theta_initialization(data_train_0_1),1)
# Apply the accuracy function
accuracy(data_train_0_1,true_label_train_0_1,theta_train_0_1)

# Training train_3_5 

# Binarize the labels
true_label_train_3_5[which(true_label_train_3_5 == 3)] = 0
true_label_train_3_5[which(true_label_train_3_5 == 5)] = 1

# Compute theta 
theta_train_3_5= gradient_descent(data_train_3_5,true_label_train_3_5,Conv.Criterion,Conv.Criterion2,number_iterations, alpha,theta_initialization(data_train_3_5),1)
# Apply the accuracy function
accuracy(data_train_3_5,true_label_train_3_5,theta_train_3_5)

# Testing test_0_1

# Apply the accuracy function with the theta computed in the training (train_0_1)
accuracy(data_test_0_1,true_label_test_0_1,theta_train_0_1)

# Testing test_3_5 

# Binarize the labels
true_label_test_3_5[which(true_label_test_3_5 == 3)] = 0
true_label_test_3_5[which(true_label_test_3_5 == 5)] = 1

# Apply the accuracy function with the theta computed in the training (train_3_5)
accuracy(data_test_3_5,true_label_test_3_5,theta_train_3_5)


# Repeat 10 times 

# Function allowing to reduce the dataframe's size by doing a shuffle split of the data
splitdataframe <- function(dataframe,percent) {
  index =  1:nrow(dataframe)
  trainig_index = sample(index, trunc((percent/100)*dim(dataframe)[1]))
  return(dataframe[trainig_index, ])
}

# Function that computes the training and testing accuracies on 10 iterations no matter the dataset
# Inputs : percent_split : percentage of split
#          output_type : c(0,1) or c(3,5)
#          conv_criterion : threshold for the first convergence criterion
#          conv_criterion2 : threshold for the second convergence criterion
#          max_iter : number maximum of iterations in order to stop in the case of a non convergence
#          learning_rate : alpha initial
#          theta  : theta_initialization or theta_variation (2 choices of initialization weights vector)
#          data_test : test_0_1 or test_3_5
#          label_test : true_label_test_0_1 or true_label_test_3_5
#          type : 1 => first convergence criteria and 2 => second convergence criteria
# Outputs : accuracy : matrix of two row and 10 colums : first row => train accuracy
#                                                        second row => test accuracy
ten_times_training_accuracy <- function(percent_split,output_type,conv_criterion,conv_criterion2,
                                        max_iter,learning_rate,theta_initialization,
                                        data_test,label_test,type){
  data_1 = train[which(train[785,] %in% output_type)] 
  label_test[which(label_test == output_type[1])] = 0
  label_test[which(label_test == output_type[2])] = 1
  accuracy_train = c()
  accuracy_test = c()
  for (i in seq(1,10)){
    print(i)
    data_2 = splitdataframe(t(data_1),percent_split)
    label_data = data_2[,785]
    label_data[which(label_data == output_type[1])] = 0
    label_data[which(label_data == output_type[2])] = 1
    data = data_2[,1:784]
    data_dataset = as.matrix(data)
    label_data = as.vector(label_data)
    theta = gradient_descent(data_dataset,label_data,conv_criterion,conv_criterion2,max_iter, learning_rate,theta_initialization(data_dataset),type)
    accuracy_train = c(accuracy_train,accuracy(data_dataset,label_data,theta))
    accuracy_test = c(accuracy_test,accuracy(data_test,label_test,theta))
    accuracy = rbind(accuracy_train,accuracy_test) 
  }
  return(accuracy)
}

# Apply the function on the 0/1 classification
accuracy_0_1 = ten_times_training_accuracy(80,c(0,1),Conv.Criterion,Conv.Criterion2,number_iterations,
                                           alpha,theta_initialization,data_test_0_1,true_label_test_0_1,1)
# Apply the function on the 3/5 classification
accuracy_3_5 = ten_times_training_accuracy(80,c(3,5),Conv.Criterion,Conv.Criterion2,number_iterations,
                                           alpha,theta_initialization,data_test_3_5,true_label_test_3_5,1)




# Question 4 ----------------------------------------------------------------

# a. Variation on initialization parameters

theta_variation <- function(dataset){
  values = floor(runif(dim(dataset)[2]/2,min = 1, max = dim(dataset)[2] ))
  theta = matrix(0, 1, dim(dataset)[2]+1)
  theta[values] = 1
  return(theta)
}

# Compute the theta with the change in the initial weights vector
theta_train_3_5_init= gradient_descent(data_train_3_5,true_label_train_3_5,Conv.Criterion,Conv.Criterion2,number_iterations, alpha,theta_variation(data_train_3_5),1)

# Compute the training accuracy for one iteration to have an idea of the effect
accuracy(data_train_3_5,true_label_train_3_5,theta_train_3_5_init)
# Compute the testing accuracy for one iteration to have an idea of the effect
accuracy(data_test_3_5,true_label_test_3_5,theta_train_3_5_init)

# Compute the training and testing accuracies for 10 iterations
accuracy_3_5_init = ten_times_training_accuracy(80,c(3,5),Conv.Criterion,Conv.Criterion2,number_iterations,
                                           alpha,theta_variation,data_test_3_5,true_label_test_3_5,1)


# b. Variation on convergence criterion

# Compute the theta with the change in the convergence criteria
theta_train_3_5= gradient_descent(data_train_3_5,true_label_train_3_5,Conv.Criterion,Conv.Criterion2,number_iterations, alpha,theta_initialization(data_train_3_5),2)
# Compute the training accuracy for one iteration to have an idea of the effect
accuracy(data_train_3_5,true_label_train_3_5,theta_train_3_5)
# Compute the testing accuracy for one iteration to have an idea of the effect
accuracy(data_test_3_5,true_label_test_3_5,theta_train_3_5)

# Compute the training and testing accuracies for 10 iterations
accuracy_3_5 = ten_times_training_accuracy(80,c(3,5),Conv.Criterion,Conv.Criterion2,number_iterations,
                                           alpha,theta_initialization,data_test_3_5,true_label_test_3_5,2)


 # Question 5 ----------------------------------------------------------------

#a.

# Computes the results of the average of the 10 iterations accuracies (training and testing) on different datset's size 
# 5%, 10% .... 100% 
percent_split = seq(5,100,by = 5)
accuracy_0_1_train = c()
accuracy_0_1_test = c()
accuracy_3_5_train = c()
accuracy_3_5_test = c()
for (i in seq(1,20)){
  accuracy_0_1 = ten_times_training_accuracy(percent_split[i],c(0,1),Conv.Criterion,Conv.Criterion2,number_iterations,
                                             alpha,theta_initialization,data_test_0_1,true_label_test_0_1,1)
  accuracy_0_1_train = c(accuracy_0_1_train,mean(accuracy_0_1[1,]))
  accuracy_0_1_test = c(accuracy_0_1_test,mean(accuracy_0_1[2,]))
  accuracy_3_5 = ten_times_training_accuracy(percent_split[i],c(3,5),Conv.Criterion,Conv.Criterion2,number_iterations,
                                             alpha,theta_initialization,data_test_3_5,true_label_test_3_5,1)
  accuracy_3_5_train = c(accuracy_3_5_train,mean(accuracy_3_5[1,]))
  accuracy_3_5_test = c(accuracy_3_5_test,mean(accuracy_3_5[2,]))
}
# accuracy (training on the first row ans testing on the second) for classification 0/1
accur_0_1 = rbind(accuracy_0_1_train,accuracy_0_1_test)
# accuracy (training on the first row ans testing on the second) for classification 3/5
accur_3_5 = rbind(accuracy_3_5_train,accuracy_3_5_test)

#b.

# Computes the logistic loss function
logistic_loss <- function(X,Y,theta){
  # add a a line of ones for c 
  X = cbind(X,rep(1,dim(X)[1]))
  m  = dim(X)[1]
  # Computes the probabilitues of the labels 0 or 1
  h = sigmoid_function(X %*% as.vector(theta))
  # Computes the loss function 
  J <- (1/m)*sum((-Y*log(h)) - ((1-Y)*log(1-h)))
  return(J)
}

# Function that computes the training and testing logistic loss on 10 iterations no matter the dataset
# Inputs : percent_split : percentage of split
#          output_type : c(0,1) or c(3,5)
#          conv_criterion : threshold for the first convergence criterion
#          conv_criterion2 : threshold for the second convergence criterion
#          max_iter : number maximum of iterations in order to stop in the case of a non convergence
#          learning_rate : alpha initial
#          theta  : theta_initialization or theta_variation (2 choices of initialization weights vector)
#          data_test : test_0_1 or test_3_5
#          label_test : true_label_test_0_1 or true_label_test_3_5
#          type : 1 => first convergence criteria and 2 => second convergence criteria
# Outputs : logistic loss : matrix of two row and 10 colums : first row => train logistic loss
#                                                             second row => test logistic loss

ten_times_training_loss <- function(percent_split,output_type,conv_criterion,conv_criterion2, 
                                        max_iter,learning_rate,theta_initialization,
                                        data_test,label_test,type){
  data_1 = train[which(train[785,] %in% output_type)]
  label_test[which(label_test == output_type[1])] = 0
  label_test[which(label_test == output_type[2])] = 1
  J_train = c()
  J_test = c()
  J = c()
  for (i in seq(1,10)){
    print(i)
    data_2 = splitdataframe(t(data_1),percent_split)
    label_data = data_2[,785]
    label_data[which(label_data == output_type[1])] = 0
    label_data[which(label_data == output_type[2])] = 1
    data = data_2[,1:784]
    data_dataset = as.matrix(data)
    label_data = as.vector(label_data)
    theta = gradient_descent(data_dataset,label_data,conv_criterion,conv_criterion2,  max_iter, learning_rate,theta_initialization(data_dataset),type)
    J_train = c(J_train,logistic_loss(data_dataset,label_data,theta))
    J_test = c(J_test,logistic_loss(data_test,label_test,theta))
    J = rbind(J_train,J_test) 
  }
  return(J)
}

# Computes the results of the average of the 10 iterations logistic loss (training and testing) on different datset's size 
# 5%, 10% .... 100% 
percent_split = seq(5,100,by = 5)
J_0_1_train = c()
J_0_1_test = c()
J_3_5_train = c()
J_3_5_test = c()
for (i in seq(1,20)){
  print(i)
  J_0_1 = ten_times_training_loss(percent_split[i],c(0,1),Conv.Criterion,Conv.Criterion2,number_iterations,
                                             alpha,theta_initialization,data_test_0_1,true_label_test_0_1,1)
  J_0_1_train = c(J_0_1_train,mean(J_0_1[1,]))
  J_0_1_test = c(J_0_1_test,mean(J_0_1[2,]))
  J_3_5 = ten_times_training_loss(percent_split[i],c(3,5),Conv.Criterion,Conv.Criterion2,number_iterations,
                                             alpha,theta_initialization,data_test_3_5,true_label_test_3_5,1)
  J_3_5_train = c(J_3_5_train,mean(J_3_5[1,]))
  J_3_5_test = c(J_3_5_test,mean(J_3_5[2,]))
}
# logistic loss (training on the first row ans testing on the second) for classification 0/1
J_0_1 = rbind(J_0_1_train,J_0_1_test)
# logistic loss (training on the first row ans testing on the second) for classification 3/5
J_3_5 = rbind(J_3_5_train,J_3_5_test)

# Graphics

# Learning curve of the accuracy for the classification 0/1
x = c(seq(5,100,by = 5),seq(5,100,by = 5))
data_split = as.data.frame(c(Train = accur_0_1[1,], Test = accur_0_1[2,]))
data_split$Type = c(rep("Train",1,20),rep("Test",1,20))
names(data_split) = c('data','Type')
ggplot(data_split,aes(x=x,y=data,col=Type))+geom_line()+ggtitle('Accuracy over different sizes of dataset for 0 and 1')+xlab('Dataset Sizes') + ylab('Correctly classified (%)')

# Learning curve of the accuracy for the classification 3/5
x = c(seq(5,100,by = 5),seq(5,100,by = 5))
data_split = as.data.frame(c(Train = accur_3_5[1,], Test = accur_3_5[2,]))
data_split$Type = c(rep("Train",1,20),rep("Test",1,20))
names(data_split) = c('data','Type')
ggplot(data_split,aes(x=x,y=data,col=Type))+geom_line()+ggtitle('Accuracy over different sizes of dataset for 3 and 5')+xlab('Dataset Sizes') + ylab('Correctly classified (%)')

# Learning curve of the logistic loss for the classification 0/1
x = c(seq(5,100,by = 5),seq(5,100,by = 5))
data_J_split = as.data.frame(c(Train = J_0_1[1,], Test = J_0_1[2,]))
data_J_split$Type = c(rep("Train",1,20),rep("Test",1,20))
names(data_J_split) = c('data','Type')
ggplot(data_J_split,aes(x=x,y=data,col=Type))+geom_line()+ggtitle('Logistic loss over different sizes of dataset for 0 and 1')+xlab('Dataset Sizes') + ylab('Loss function')

# Learning curve of the logistic loss for the classification 3/5
x = c(seq(5,100,by = 5),seq(5,100,by = 5))
data_J_split = as.data.frame(c(Train = J_3_5[1,], Test = J_3_5[2,]))
data_J_split$Type = c(rep("Train",1,20),rep("Test",1,20))
names(data_J_split) = c('data','Type')
ggplot(data_J_split,aes(x=x,y=data,col=Type))+geom_line()+ggtitle('Logistic loss over different sizes of dataset for 3 and 5')+xlab('Dataset Sizes') + ylab('Loss function')
 




