Title: Determine manner in which exercise is performed based on Machine Learning Technique
========================================================

## Executive Summary
Computer devices (such as Nike Fueland, Fitbit and even smart phone) the data collection about personal activity relatively inexpensively.   Studies (from http://groupware.les.inf.puc-rio.br/har) were performed asking 6 participants to lift the dumbbell correctly and incorrectly in 5 different ways (hereafter, known as class).  Data were obtained from accelerometers on the belt, forearm, arm, and dumbbell of the 6 participants.   Two data set, a large training set and a testing set containing 20 observations were provided.

This paper describe the process used to select the random forest as the machine learning method through different training methods.   The random forest method thus obtained accurately predict the exercise class for all the 20 cases.


## Data Processing
### Raw data analysis and processing

The training set contains 160 columns and 19622 rows.   The classe column is a factor variable containing 5 different type as follows.
- A: correctly (per specification)
- B:  throwing elbow to front
- C:  lifting dumbbell half way
-	D:  Lowering dumbbell half way
-	E:  Throwing hip to front.

Steps are required to refine the dataset variable to a more manageable size.   Four different types of variables (columns) are of no value in selecting the machine learning methods and will be eliminated.  They are

1. Descriptive, non-relevant variables - e.g. user name, timestamps, number of windows.
2. Variables with missing no data - e.g. kurtosis, skewness data
3. Variable with NA data - e.g.  standard deviation,  variance, average, maximum and minimum data
4. Variable that are highly correlated - (using more of these variables will increase the processing time without improving accuracy).    A correlation coefficient of 0.75 or higher is used to screen off highly correlated variable.   Examples of correlated data are roll_belt vs, yaw_belt, total_accl_belt, accel_belt_y, accel_belt_z, and accel_arm_y.


After this screening, a total number of 36 columns (including classe) was obtained.  This were written to a file called "corColumn" to enable easy data subsetting later.   See  screening and train data Subsetting below.




### screening and train data Subsetting
   

```r
setwd("E://courses//jh-dataexplore//ws_machine")
d <- read.csv("pml-training.csv")


# get rid of those column that has lots of NA and no data.  90% is
# arbitrarily chosen to indicate a lot
s <- sapply(d, function(x) (sum(is.na(x))/nrow(d) > 0.9) || (nrow(d[x == "", 
    ])/nrow(d) > 0.9))
s1 <- for (i in 1:length(s)) {
    if (s[i] == FALSE) 
        print(paste(i, names(s)[i], sep = " "))
}
```

```
## [1] "1 X"
## [1] "2 user_name"
## [1] "3 raw_timestamp_part_1"
## [1] "4 raw_timestamp_part_2"
## [1] "5 cvtd_timestamp"
## [1] "6 new_window"
## [1] "7 num_window"
## [1] "8 roll_belt"
## [1] "9 pitch_belt"
## [1] "10 yaw_belt"
## [1] "11 total_accel_belt"
## [1] "37 gyros_belt_x"
## [1] "38 gyros_belt_y"
## [1] "39 gyros_belt_z"
## [1] "40 accel_belt_x"
## [1] "41 accel_belt_y"
## [1] "42 accel_belt_z"
## [1] "43 magnet_belt_x"
## [1] "44 magnet_belt_y"
## [1] "45 magnet_belt_z"
## [1] "46 roll_arm"
## [1] "47 pitch_arm"
## [1] "48 yaw_arm"
## [1] "49 total_accel_arm"
## [1] "60 gyros_arm_x"
## [1] "61 gyros_arm_y"
## [1] "62 gyros_arm_z"
## [1] "63 accel_arm_x"
## [1] "64 accel_arm_y"
## [1] "65 accel_arm_z"
## [1] "66 magnet_arm_x"
## [1] "67 magnet_arm_y"
## [1] "68 magnet_arm_z"
## [1] "84 roll_dumbbell"
## [1] "85 pitch_dumbbell"
## [1] "86 yaw_dumbbell"
## [1] "102 total_accel_dumbbell"
## [1] "113 gyros_dumbbell_x"
## [1] "114 gyros_dumbbell_y"
## [1] "115 gyros_dumbbell_z"
## [1] "116 accel_dumbbell_x"
## [1] "117 accel_dumbbell_y"
## [1] "118 accel_dumbbell_z"
## [1] "119 magnet_dumbbell_x"
## [1] "120 magnet_dumbbell_y"
## [1] "121 magnet_dumbbell_z"
## [1] "122 roll_forearm"
## [1] "123 pitch_forearm"
## [1] "124 yaw_forearm"
## [1] "140 total_accel_forearm"
## [1] "151 gyros_forearm_x"
## [1] "152 gyros_forearm_y"
## [1] "153 gyros_forearm_z"
## [1] "154 accel_forearm_x"
## [1] "155 accel_forearm_y"
## [1] "156 accel_forearm_z"
## [1] "157 magnet_forearm_x"
## [1] "158 magnet_forearm_y"
## [1] "159 magnet_forearm_z"
## [1] "160 classe"
```

```r

# column_N is a file obtained by manually eliminating irrelevant data such
# as user_name, timestamp etc
colN <- read.table("column_N.txt")

d.num <- subset(d, select = c(colN$V1))
# give the subset data meaningful column names
names(d.num) <- colN$V2



# Eliminating correlated data correlation between column can be found as
# below this find the correlation between other columns with the columns
# roll_belt.  Write a function to go through every column of the set
sapply(d.num, cor, y = d.num$roll_belt)
```

```
##            roll_belt           pitch_belt             yaw_belt 
##             1.000000            -0.215925             0.815230 
##     total_accel_belt         gyros_belt_x         gyros_belt_y 
##             0.980924            -0.117470             0.463718 
##         gyros_belt_z         accel_belt_x         accel_belt_y 
##            -0.459038             0.256835             0.924898 
##         accel_belt_z        magnet_belt_x        magnet_belt_y 
##            -0.992009             0.352584            -0.211267 
##        magnet_belt_z             roll_arm            pitch_arm 
##            -0.066378            -0.371983             0.059771 
##              yaw_arm      total_accel_arm          gyros_arm_x 
##            -0.225899            -0.278261             0.029119 
##          gyros_arm_y          gyros_arm_z          accel_arm_x 
##            -0.227514             0.546794             0.230557 
##          accel_arm_y          accel_arm_z         magnet_arm_x 
##            -0.794781             0.389815             0.088874 
##         magnet_arm_y         magnet_arm_z        roll_dumbbell 
##             0.009562             0.023091            -0.126492 
##       pitch_dumbbell         yaw_dumbbell total_accel_dumbbell 
##             0.064034             0.025641            -0.192066 
##     gyros_dumbbell_x     gyros_dumbbell_y     gyros_dumbbell_z 
##             0.009558             0.057964             0.003227 
##     accel_dumbbell_x     accel_dumbbell_y     accel_dumbbell_z 
##             0.218708            -0.263810             0.105521 
##    magnet_dumbbell_x    magnet_dumbbell_y    magnet_dumbbell_z 
##             0.312808            -0.286466            -0.499921 
##         roll_forearm        pitch_forearm          yaw_forearm 
##            -0.150169             0.174615            -0.266430 
##  total_accel_forearm      gyros_forearm_x      gyros_forearm_y 
##             0.065555             0.381044             0.048512 
##      gyros_forearm_z      accel_forearm_x      accel_forearm_y 
##             0.025705            -0.485450             0.030824 
##      accel_forearm_z     magnet_forearm_x     magnet_forearm_y 
##             0.081950            -0.191801             0.029504 
##     magnet_forearm_z 
##             0.272597
```

```r

# Any column with correlation greater than 0.75 is eliminated.  This
# eliminated column is removed from the column_N.txt file and written to
# another file called coColumn.txt -- forming the final processed data set
colD <- read.table("corColumn.txt")
```


### Data Subsetting
The corColumn file content include (second column is the column name in the training data set, column one indicate column position, i.e.  Roll_belt is the 8th column in the original training data set)
<PRE>
8   roll_belt
9	  pitch_belt
37	gyros_belt_x
38	gyros_belt_y
39	gyros_belt_z
44	magnet_belt_y
46	roll_arm
47	pitch_arm
48	yaw_arm
49	total_accel_arm
60	gyros_arm_x
62	gyros_arm_z
64	accel_arm_y
65  accel_arm_z
66	magnet_arm_x
67	magnet_arm_y
84	roll_dumbbell
85	pitch_dumbbell
86	yaw_dumbbell
113	gyros_dumbbell_x
114	gyros_dumbbell_y
116	accel_dumbbell_x
117	accel_dumbbell_y
118	accel_dumbbell_z
121	magnet_dumbbell_z
122	roll_forearm
123	pitch_forearm
124	yaw_forearm
140	total_accel_forearm
151	gyros_forearm_x
154	accel_forearm_x
155	accel_forearm_y
156	accel_forearm_z
157	magnet_forearm_x
159	magnet_forearm_z
160	classe
</PRE>

    The orginal training dataset is subset using the above list of columns. 


## Training Methods used

```r
# note: colDerv$V1 is the column position colDerv$V2 is the columnn Name

d.sub <- subset(d, select = c(colD$V1))

# give the subset data meaningful column names
names(d.sub) <- colD$V2

set.seed(1234)
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
inTrain <- createDataPartition(y = d.sub$classe, p = 0.5, list = FALSE)
training <- d.sub[inTrain, ]
testing <- d.sub[-inTrain, ]
```





```r
Sys.time()
```

```
## [1] "2014-07-22 09:51:24 EST"
```

```r
mod.gbm <- train(classe ~ ., method = "gbm", data = training, verbose = FALSE)
```

```
## Loading required package: gbm
```

```
## Warning: package 'gbm' was built under R version 3.1.1
```

```
## Loading required package: survival
## Loading required package: splines
## 
## Attaching package: 'survival'
## 
## The following object is masked from 'package:caret':
## 
##     cluster
## 
## Loading required package: parallel
## Loaded gbm 2.1
## Loading required package: plyr
```

```
## Warning: package 'e1071' was built under R version 3.1.1
```

```r
Sys.time()
```

```
## [1] "2014-07-22 10:04:05 EST"
```

```r
mod.rf <- train(classe ~ ., method = "rf", data = training, trControl = trainControl(method = "cv"), 
    number = 3)
```

```
## Loading required package: randomForest
```

```
## Warning: package 'randomForest' was built under R version 3.1.1
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
Sys.time()
```

```
## [1] "2014-07-22 10:13:51 EST"
```

```r


pred.gbm <- predict(mod.gbm, testing)
pred.rf <- predict(mod.rf, testing)
```


Caret package is used to subset the training into training and validation dataset using 50%-50% split.

Two training methods were employed, gbm (generalized boosted model) and rf (random forest).  Figure 1 is an example showing the predicted outcome for each of the 5 classe using the gbm method.   


Fig1: Using validation dataset to estimate gbm model predicated accuracy of the classes 

```r
qplot(pred.gbm, colour = classe, fill = classe, data = testing)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


The predicted value is cross-validated with the real result in the validation set and tabulated as follows
<PRE>
pred.gbm    A    B    C    D    E
       A 2731   77    4    3    2
       B   28 1737   79    7   29
       C   11   67 1593   90   40
       D   15   14   33 1500   32
       E    5    3    2    8 1700

pred.rf    A    B    C    D    E
      A 2788   19    0    0    0
      B    1 1868   26    0    0
      C    1   11 1676   47    4
      D    0    0    9 1560    2
      E    0    0    0    1 1797
</PRE>


## Cross-validation
Cursory viewing shows that random forest provides a more accurate prediction.    Accuracy and Kappa statistics also show random forest outperforms generalized boosted method.
<Table border="1" bgcolor="#FF0000">
<TR><TD>Generalized</TD><TD>Boosted Method(1)</TD><TD>Random Forest(2)</TD></TR>
<TR><TD>Training time (3)</TD><TD>13 minutes</TD><TD>9 minutes</TD></TR>
<TR><TD>Accuracy</TD><TD>0.936</TD><TD>0.983</TD></TR>
<TR><TD>Kappa</TD><TD>0.919</TD><TD>0.979</TD></TR>
<TR><TD>Accuracy SD</TD><TD>0.0044</TD><TD>0.0034</TD></TR>
<TR><TD>Kappa SD</TD><TD>0.0055</TD><TD>0.0043</TD></TR>
</Table>

Kappa is a measure of agreement between predicted presences and absences with actual presences and absences corrected for agreement that might be due to chance alone.
(1)	Quoting statistics of gbm with interaction.depth of 3, n.tree of 150
(2)	Quoting statistics of rf using mtry = 18
(3) time obtained using Sys.time() function on my computer.


The performance can be visualized easily in the following diagram.  Each blue dot represent the prediction accuracy of each classe, in order of A, B, C, D, E.   Using the first point as an example, gbm predict with 91% accuracy, whereas random forest predict 98.5%.  The red line is a 45 degree slope.  Thus any point to the left of it has a high degree of accuracy, i.e. random forest outperform gbm on all fronts.




```r
table.gbm <- table(pred.gbm, testing$classe)  # predict value is row,  testcase value is column
table.rf <- table(pred.rf, testing$classe)

# cross-validation in terms of percentage prop.table(table.rf,2) # get the
# column frequency of occurrence


diag.gbm <- diag(prop.table(table.gbm, 2))
diag.rf <- diag(prop.table(table.rf, 2))


# draw a graphs of showing accuracy of diag1 and diag2
diag.comb <- cbind(diag.gbm, diag.rf)
```




```r
par(mfrow = c(1, 1))
with(testing, plot(diag.comb * 100, pch = 19, cex = 1, col = "blue", main = "compare training methods", 
    xlab = "%accuracy using gbm", ylab = "% accuracy - random forest"))
abline(0, 1, lwd = 2, col = "red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 





```r

inTrain <- createDataPartition(y = d.sub$classe, p = 0.7, list = FALSE)
training <- d.sub[inTrain, ]
testing <- d.sub[-inTrain, ]
Sys.time()
```

```
## [1] "2014-07-22 10:13:52 EST"
```

```r
mod.rf7 <- train(classe ~ ., method = "rf", data = training, trControl = trainControl(method = "cv"), 
    number = 3)
Sys.time()
```

```
## [1] "2014-07-22 10:28:01 EST"
```

```r


pred.rf7 <- predict(mod.rf7, testing)
table.rf7 <- table(pred.rf7, testing$classe)
diag.rf7 <- diag(prop.table(table.rf7, 2))

diag.rfcomb <- cbind(diag.rf, diag.rf7)
```


Random forest is chosen this time using a larger training dataset with 70-30 split for training and validation.  Training time increase to 14 minutes. The accuracy comparison is present below graphically.  This show the 70-30 split provide marginally better accuracy than the 50-50 split.

```r
par(mfrow = c(1, 1))
with(testing, plot(diag.rfcomb, pch = 19, cex = 1, col = "blue", main = "compare random forest", 
    ylab = "%accur- 70% train data", xlab = "%accur-50% train data"))
abline(0, 1, lwd = 2, col = "red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


## Conclusion
### Prediction on the provided dataset
Using the same method describe under raw data processing, remove the irrelevant column in the test data set.  Add a column called number to identify the observation number.   Then apply the prediction model (obtained from random forest with 70-30 split in training data) to each of the observation. The code below shows how this was done.    That said, other experiments also conducted, that show the gbm, or random forest at even 50:50 split of training also provides every accurate prediction.


```r
# note: colDerv$V1 is the column position colDerv$V2 is the columnn Name
t <- read.csv("pml-testing.csv")
t.sub <- subset(t, select = c(colD$V1))

# give the subset data meaningful column names
names(t.sub) <- colD$V2

t.sub$number <- c(1:20)
# subsetting

for (i in 1:20) {
    print(paste(i, predict(mod.rf7, t.sub[t.sub$number == i, ]), sep = "  "))
}
```

```
## [1] "1  B"
## [1] "2  A"
## [1] "3  B"
## [1] "4  A"
## [1] "5  A"
## [1] "6  E"
## [1] "7  D"
## [1] "8  B"
## [1] "9  A"
## [1] "10  A"
## [1] "11  B"
## [1] "12  C"
## [1] "13  B"
## [1] "14  A"
## [1] "15  E"
## [1] "16  E"
## [1] "17  A"
## [1] "18  B"
## [1] "19  B"
## [1] "20  B"
```




