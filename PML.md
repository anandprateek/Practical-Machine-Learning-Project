# Practical Machine Learning Project

## Introduction

### Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

### Data 
The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 

## Importing Libraries

```r
library(caret)
library(knitr)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
```

## Reading the data

```r
training <- read.csv("pml-training.csv")
testing<- read.csv("pml-testing.csv")
```


```r
countna<-sapply(training,function(x) sum(is.na(x)))
training<-training[,ifelse(countna==0,TRUE,FALSE)]
#As the first seven columns has data that is of no use, we remove them
training<-training[,-c(1:7)]
```

## Dividing data into training and test datasets

```r
divdata<-createDataPartition(training$classe,p=0.6,list=FALSE)
train_data<-training[divdata,]
test_data<-training[-divdata,]
head(test_data)
```

```
##   roll_belt pitch_belt yaw_belt total_accel_belt kurtosis_roll_belt kurtosis_picth_belt kurtosis_yaw_belt skewness_roll_belt skewness_roll_belt.1
## 1      1.41       8.07    -94.4                3                                                                                                 
## 2      1.41       8.07    -94.4                3                                                                                                 
## 3      1.42       8.07    -94.4                3                                                                                                 
## 5      1.48       8.07    -94.4                3                                                                                                 
## 7      1.42       8.09    -94.4                3                                                                                                 
## 9      1.43       8.16    -94.4                3                                                                                                 
##   skewness_yaw_belt max_yaw_belt min_yaw_belt amplitude_yaw_belt gyros_belt_x gyros_belt_y gyros_belt_z accel_belt_x accel_belt_y accel_belt_z
## 1                                                                        0.00         0.00        -0.02          -21            4           22
## 2                                                                        0.02         0.00        -0.02          -22            4           22
## 3                                                                        0.00         0.00        -0.02          -20            5           23
## 5                                                                        0.02         0.02        -0.02          -21            2           24
## 7                                                                        0.02         0.00        -0.02          -22            3           21
## 9                                                                        0.02         0.00        -0.02          -20            2           24
##   magnet_belt_x magnet_belt_y magnet_belt_z roll_arm pitch_arm yaw_arm total_accel_arm gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x accel_arm_y
## 1            -3           599          -313     -128      22.5    -161              34        0.00        0.00       -0.02        -288         109
## 2            -7           608          -311     -128      22.5    -161              34        0.02       -0.02       -0.02        -290         110
## 3            -2           600          -305     -128      22.5    -161              34        0.02       -0.02       -0.02        -289         110
## 5            -6           600          -302     -128      22.1    -161              34        0.00       -0.03        0.00        -289         111
## 7            -4           599          -311     -128      21.9    -161              34        0.00       -0.03        0.00        -289         111
## 9             1           602          -312     -128      21.7    -161              34        0.02       -0.03       -0.02        -288         109
##   accel_arm_z magnet_arm_x magnet_arm_y magnet_arm_z kurtosis_roll_arm kurtosis_picth_arm kurtosis_yaw_arm skewness_roll_arm skewness_pitch_arm
## 1        -123         -368          337          516                                                                                           
## 2        -125         -369          337          513                                                                                           
## 3        -126         -368          344          513                                                                                           
## 5        -123         -374          337          506                                                                                           
## 7        -125         -373          336          509                                                                                           
## 9        -122         -369          341          518                                                                                           
##   skewness_yaw_arm roll_dumbbell pitch_dumbbell yaw_dumbbell kurtosis_roll_dumbbell kurtosis_picth_dumbbell kurtosis_yaw_dumbbell
## 1                       13.05217      -70.49400    -84.87394                                                                     
## 2                       13.13074      -70.63751    -84.71065                                                                     
## 3                       12.85075      -70.27812    -85.14078                                                                     
## 5                       13.37872      -70.42856    -84.85306                                                                     
## 7                       13.12695      -70.24757    -85.09961                                                                     
## 9                       13.15463      -70.42520    -84.91563                                                                     
##   skewness_roll_dumbbell skewness_pitch_dumbbell skewness_yaw_dumbbell max_yaw_dumbbell min_yaw_dumbbell amplitude_yaw_dumbbell total_accel_dumbbell
## 1                                                                                                                                                 37
## 2                                                                                                                                                 37
## 3                                                                                                                                                 37
## 5                                                                                                                                                 37
## 7                                                                                                                                                 37
## 9                                                                                                                                                 37
##   gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x magnet_dumbbell_y
## 1                0            -0.02                0             -234               47             -271              -559               293
## 2                0            -0.02                0             -233               47             -269              -555               296
## 3                0            -0.02                0             -232               46             -270              -561               298
## 5                0            -0.02                0             -233               48             -270              -554               292
## 7                0            -0.02                0             -232               47             -270              -551               295
## 9                0            -0.02                0             -232               47             -269              -549               292
##   magnet_dumbbell_z roll_forearm pitch_forearm yaw_forearm kurtosis_roll_forearm kurtosis_picth_forearm kurtosis_yaw_forearm skewness_roll_forearm
## 1               -65         28.4         -63.9        -153                                                                                        
## 2               -64         28.3         -63.9        -153                                                                                        
## 3               -63         28.3         -63.9        -152                                                                                        
## 5               -68         28.0         -63.9        -152                                                                                        
## 7               -70         27.9         -63.9        -152                                                                                        
## 9               -65         27.7         -63.8        -152                                                                                        
##   skewness_pitch_forearm skewness_yaw_forearm max_yaw_forearm min_yaw_forearm amplitude_yaw_forearm total_accel_forearm gyros_forearm_x
## 1                                                                                                                    36            0.03
## 2                                                                                                                    36            0.02
## 3                                                                                                                    36            0.03
## 5                                                                                                                    36            0.02
## 7                                                                                                                    36            0.02
## 9                                                                                                                    36            0.03
##   gyros_forearm_y gyros_forearm_z accel_forearm_x accel_forearm_y accel_forearm_z magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
## 1            0.00           -0.02             192             203            -215              -17              654              476      A
## 2            0.00           -0.02             192             203            -216              -18              661              473      A
## 3           -0.02            0.00             196             204            -213              -18              658              469      A
## 5            0.00           -0.02             189             206            -214              -17              655              473      A
## 7            0.00           -0.02             195             205            -215              -18              659              470      A
## 9            0.00           -0.02             193             204            -214              -16              653              476      A
```

## Model Training and predicting

```r
model<-train(data=train_data,classe~yaw_belt+accel_belt_z+gyros_arm_x+accel_arm_x+roll_dumbbell+gyros_dumbbell_x)
predictTrain<-predict(model,train_data)
print(paste("Ratio correct on train_data=",sum(predictTrain==train_data$classe)/length(predictTrain)))
```

```
## [1] "Ratio correct on train_data= 1"
```

```r
predictTest<-predict(model,test_data)
print(paste("Ratio correct on test_data=",sum(predictTest==train_data$classe)/length(predictTest)))
```

```
## Warning in `==.default`(predictTest, train_data$classe): longer object length is not a multiple of shorter object length
```

```
## Warning in is.na(e1) | is.na(e2): longer object length is not a multiple of shorter object length
```

```
## [1] "Ratio correct on test_data= 0.335457557991333"
```

## Plots

```r
featurePlot(x = training[,c(8:10)], y = training$classe, plot = "box")
```

```
## NULL
```

```r
ggplot(data = training, aes(x = accel_belt_z, fill = classe)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
ggplot(data = training, aes(x = gyros_arm_x, fill = classe)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-2.png)

```r
ggplot(data = training, aes(x = accel_arm_x, fill = classe)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-3.png)

```r
plot(model)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-4.png)

## Analysis

```r
dim(train_data)
```

```
## [1] 11776    86
```

```r
dim(test_data)
```

```
## [1] 7846   86
```

```r
reqdata<-as.factor(test_data$classe)
AccuracyTest<-confusionMatrix(predictTest,reqdata)
AccuracyTest
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2118   58   38   43    9
##          B   16 1391   30    6   11
##          C   39   45 1271   46    6
##          D   43   17   25 1180   16
##          E   16    7    4   11 1400
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9381          
##                  95% CI : (0.9325, 0.9433)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9216          
##                                           
##  Mcnemar's Test P-Value : 6.114e-06       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9489   0.9163   0.9291   0.9176   0.9709
## Specificity            0.9736   0.9900   0.9790   0.9846   0.9941
## Pos Pred Value         0.9347   0.9567   0.9033   0.9212   0.9736
## Neg Pred Value         0.9796   0.9801   0.9849   0.9839   0.9934
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2699   0.1773   0.1620   0.1504   0.1784
## Detection Prevalence   0.2888   0.1853   0.1793   0.1633   0.1833
## Balanced Accuracy      0.9613   0.9532   0.9540   0.9511   0.9825
```

```r
FinalPrediction<- predict(model, testing, type = "prob")
FinalPrediction
```

```
##        A     B     C     D     E
## 1  0.136 0.610 0.124 0.116 0.014
## 2  0.738 0.084 0.054 0.008 0.116
## 3  0.098 0.578 0.176 0.028 0.120
## 4  0.988 0.006 0.006 0.000 0.000
## 5  0.736 0.050 0.196 0.000 0.018
## 6  0.038 0.132 0.038 0.040 0.752
## 7  0.050 0.080 0.130 0.606 0.134
## 8  0.262 0.280 0.184 0.096 0.178
## 9  0.988 0.004 0.000 0.000 0.008
## 10 0.954 0.004 0.014 0.020 0.008
## 11 0.068 0.868 0.004 0.010 0.050
## 12 0.046 0.006 0.918 0.018 0.012
## 13 0.002 0.976 0.008 0.008 0.006
## 14 1.000 0.000 0.000 0.000 0.000
## 15 0.032 0.018 0.032 0.146 0.772
## 16 0.004 0.016 0.014 0.088 0.878
## 17 0.968 0.004 0.002 0.000 0.026
## 18 0.034 0.818 0.000 0.046 0.102
## 19 0.258 0.294 0.412 0.030 0.006
## 20 0.012 0.976 0.008 0.000 0.004
```
Hence the accuracy of the model on test_data dataset is 93.7% which estimates the out-of-sample error to be 100-93.7=6.3%.
