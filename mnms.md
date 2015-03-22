# Random forest
    
    This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
    
    
    ```r
    library(caret)
    library(randomForest)
    #####1
    
    # # write the file url and file destination to an object
    # file.url <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
    # file.dest <- 'training.csv'
    # 
    # # download from the URL
    # download.file(file.url, file.dest, mode = "wb" )
    
    # read the data
    training <- read.csv("C:/Users/Joanna/Desktop/coursera maschine learning/assesment/training.csv")
    ```

You can also embed plots, for example:
    
    
    ```r
    #####2
    
    # # write the file url and file destination to an object
    # file.url <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
    # file.dest <- 'test.csv'
    # 
    # # download from the URL
    # download.file(file.url, file.dest,mode = "wb" )
    
    # read the data
    test <- read.csv("C:/Users/Joanna/Desktop/coursera maschine learning/assesment/test.csv")
    ```


```r
############################random forest
choose <- c(2, 8:11, 37:49, 60:68, 84:86, 102, 113:124, 140, 151:159, 160)

#removing all rows that are summaries
training <- training[training$new_window == "no",]
training2 <- training[, choose]
colnames(training2)
```

```
##  [1] "user_name"            "roll_belt"            "pitch_belt"          
##  [4] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
##  [7] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [10] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [13] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [16] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [19] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [22] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [25] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [28] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [31] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [34] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [37] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [40] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [43] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [46] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [49] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [52] "magnet_forearm_y"     "magnet_forearm_z"     "classe"
```


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


```r
####################
dim(training)
```

```
## [1] 19216   160
```

```r
dim(test)
```

```
## [1]  20 160
```

```r
#wchich columns contain NA values
#colSums(is.na(training))
#####
classe<-training2$classe
```


```r
#training and validating set
testIndex = createDataPartition(classe, p = 0.60,list=FALSE)
training3 = training2[-testIndex,]
validating = training2[testIndex,]

dim(training3)
```

```
## [1] 7684   54
```

```r
dim(validating)
```

```
## [1] 11532    54
```

```r
dim(test)
```

```
## [1]  20 160
```

```r
class(training3$classe)
```

```
## [1] "factor"
```



```r
############################random forest
library(foreach)
library(doParallel)
cl <- makePSOCKcluster(2)
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)
modFit<-train(classe~.,data=training3, method="rf", prox=TRUE)
modFit
```


```r
# saveRDS(modFit, "modFit2.rds")
modFit <- readRDS("C:\\Users\\Joanna\\Desktop\\modFit2.rds")
```



```r
############################random forest
dim(training3)
```

```
## [1] 7684   54
```

```r
dim(validating)
```

```
## [1] 11532    54
```



```r
############################random forest
predVAL<-predict(modFit, validating)
table(predVAL, validating$classe)
```

```
##        
## predVAL    A    B    C    D    E
##       A 3280   14    0    0    0
##       B    3 2201   28    0    0
##       C    0   12 1978   24    4
##       D    0    1    6 1864    4
##       E    0    3    0    1 2109
```


```r
############################random forest
pred<-predict(modFit, test)
print(pred)
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```


```r
############################random forest
answers = pred

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(answers)
```
