---
title: "Reproducible Research Project"
author: "Hassan Reza Rezaye"
date: "2026-08-26"
output: html_document
---

> Reproducible Research - Week2 - Course Project 1

### Overview:

This Project makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

``The data for this assignment can be downloaded from the course web site:`` 

> [Dataset: Activity monitoring data]([https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

####  The variables included in this dataset are:
1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
2. date: The date on which the measurement was taken in YYYY-MM-DD format
3. interval: Identifier for the 5-minute interval in which measurement was taken
 
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Question1. 
Loading and preprocessing the data

```{r,  results='hide', warning=FALSE, message=FALSE}
# To start working, prepare your working directory accurately.
#setwd("./Reproducible Research")
getwd()

# upload the necessary and relevant library which will be employed in the analysis.
library(dplyr)
library(ggplot2)

Urlproject1RR <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(Urlproject1RR, 
              destfile = "ActivityMonitoringData")
unzip("ActivityMonitoringData")
```

##### 1. Load the data (i.e. read.csv())
```{r}
Activity <- read.csv("./activity.csv")

# In general, you can get a comprehenve understanding of the row/precleaned dataset by the below two functions 
summary(Activity)
str(Activity)

# Check for NAs throughout the dataset
colSums(is.na(Activity))
# knowing the proportion of NAs in the Steps column/variable
mean(Activity$steps == 0, na.rm = TRUE)
```

##### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
# assigned the the dataframe to new object to avoid editing the row dataset
Activity1 <- Activity 

# reformated the data column to correct class (Date/as.POSICXLt) 
Activity1$date <- as.Date(Activity1$date)

str(Activity1)
```

### Question2.  
What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

##### 1. Calculate the total number of steps taken per day 

```{r}
TotalSteps <- Activity1 %>% select(steps, date) %>%
  group_by(date) %>%
  summarise(totalSt = sum(steps))
TotalSteps
```

##### 2. Make a histogram
If you do not understand the difference between a histogram and a barplot, research the difference between them. 
Make a histogram of the total number of steps taken each day

**a. histogram plot of total steps in each day:**                          
```{r, message=FALSE, warning=FALSE, fig.width=6, fig.height=3}
TotalSteps %>%  ggplot2::ggplot(aes(totalSt, fill = as.factor(date))) +
                        geom_histogram()+
                        scale_x_continuous(breaks = seq(min(TotalSteps$totalSt,na.rm = TRUE)-41, 
                                                        max(TotalSteps$totalSt,na.rm = TRUE), by = 3000))+
                        labs(fill = "date")+
                        labs(title = "Total steps in each day",
                             x = "total Steps",
                             ) + 
                         theme(plot.title = element_text(hjust = 0.5),
                               legend.position = "none"
                               )

# added density line to the histogram plot (it isnt required to be applied for the assignement) 
ggplot2::ggplot(TotalSteps, aes(x = totalSt, y = after_stat(density))) +
        geom_histogram()+
        scale_x_continuous(breaks = seq(min(TotalSteps$totalSt, na.rm = TRUE)-41, 
                                        max(TotalSteps$totalSt, na.rm = TRUE), 
                                        by = 3000))+
        geom_density(color = "green", linewidth = 1)+
        labs(
           title = "Total steps in each day",
           x = "total Steps",
           ) + 
        theme(plot.title = element_text(hjust = 0.5)
             )
```


**b. bar plot: for practice I generated bar plot as well.**
```{r,  message=FALSE, warning=FALSE, big-plot, fig.width=8, fig.height=4}
TotalSteps %>% ggplot(aes(date, totalSt, colour = date)) +
              geom_bar(stat="identity")+
              scale_x_date(date_labels = "%d-%a", 
                           date_breaks = "5 day",
                           limits = c(min(Activity1$date), max(Activity1$date)),
                           expand = c(0, 0))+
              labs(
                 title = "Total steps in each day",
                 x = "Date",
                 y = "Totol Steps/Day") + 
              theme(plot.title = element_text(hjust = 0.5)
                   )
```

##### Q3. Calculate and report the mean and median of the total number of steps taken per day  

```{r, message=FALSE, warning=FALSE}

ActMnMdn <- TotalSteps %>% select(date, totalSt)%>%
  summarise(mean = mean(totalSt, na.rm = TRUE), 
            median = median(totalSt, na.rm = TRUE))

# Note: dplyr verbs returns tibble instead of data frame
ActMnMdn

```


### Question 3. 
What is the average daily activity pattern?

###### 1. Make a time series plot
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)          
```{r}
# for time series plot, the inteval formate should be numeric. it is now factor.
# ActAveraged$interval <- as.numeric(ActAveraged$interval)
ActAveraged <- Activity1 %>% group_by(interval) %>%
  summarise(meanSt = mean(steps, na.rm = TRUE)) 

str(ActAveraged)

```
##### 2. Which 5-minute interval, on average across all the days in the dataset, contains
```{r}
# the maximum number of steps?

# ActAveraged[which.max(ActAveraged$meanSt), ]
ActAveraged %>% filter(meanSt == max(meanSt))
```
### Question 4. 
**Imputing missing values**

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs) 

```{r}
#checking for number of Nas in each column

colSums(is.na(Activity)) #to achieve the result apply()and sapply() can also be applied
```
##### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```{r}
# filter the dataset to locate where the NAs are existing 
ActMissing <- Activity1[which(is.na(Activity1$steps)),]

unique(ActMissing$date) # what days/dates, Steps variable contain missing values 
```

**a. filling the NAs with mean for that day**
```{r}
ActFillingDT <- Activity1 %>% 
                select(steps, date, interval) %>%
                group_by(date) %>%
                mutate(st = ifelse(is.na(steps),
                                   ifelse(all(is.na(steps)), 
                                          # if only one group has entirely NAs, 
                                          mean(Activity1$steps, na.rm = TRUE), 
# then get mean of variable "Steps" across the entire dataset
                                   mean(steps, na.rm = TRUE)), 
#then get mean of that group If not all values are missing (just some are NAs in a group) .
                                   steps)) # otherwise if a row in a group has valid value then keep it.
ActFillingDT
```

```{r,  eval =FALSE}
# check if the filling is done accurately in specific dates
ActFillingDT[which(ActFillingDT$date == "2012-10-01"), ]
ActFillingDT[which(ActFillingDT$date == "2012-10-19"), ]
ActFillingDT[which(ActFillingDT$date == "2012-10-08"), ]
```
**b. filling the NAs with mean for that 5-minute interval across all days**
```{r}
ActfillingInt <- Activity1 %>% select(interval, date, steps) %>%
                               group_by(interval) %>%
                               mutate(steps = ifelse(is.na(steps),
                                                      ifelse(all(is.na(steps)), 
# if only one group has entirely NAs, 
                                                             mean(Activity3$steps, na.rm = TRUE), 
# then get mean of variable "Steps" across the entire dataset
                                                       mean(steps, na.rm = TRUE)), 
# If not all are missing (juss some are NAs in a group) then get mean of that group.
                                                       steps))
ActfillingInt
```

```{r,  eval =FALSE}
# check if the filling is done accurately in specific dates
ActfillingInt[ActfillingInt$date == "2012-10-01", ]
ActfillingInt[ActfillingInt$date == "2012-10-19", ]
ActfillingInt[ActfillingInt$date == "2012-10-08", ]
```

#### 4. Make a histogram:
- Make a histrogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
- Do these values differ from the estimates from the first part of the assignment? 
- What is the impact of imputing missing data on the estimates of the total daily number of steps?

> As the question is lengthy, I partitioned each part with alphabetic character.

```{r, message=FALSE, fig.width=6, fig.height=3}
TotalStfilled <- ActfillingInt %>% group_by(date) %>%
                                summarise(totalSteps = sum(steps))

TotalStfilled # totalSteps with interval based imputed 

# a. plot the histogram
ggplot(TotalStfilled, aes(totalSteps)) +
geom_histogram()+
                labs(
                 title = "Total steps - Imputed data",
                 x = "Total Steps") + 
              theme(plot.title = element_text(hjust = 0.5)
                   )
```

```{r}
# b. mean and median of total number of steps taken per day.

# b1. Totol steps calculated by date based grouping from dateset with NAs. 
# "TotalSteps" object is calculated in the beginning. 
ActMnMdn <- TotalSteps %>% 
            summarise(mean = mean(totalSt, na.rm = TRUE), 
                      median = median(totalSt, na.rm = TRUE))
                      
# b2. Totol steps calculated by interval based grouping from dateset without NAs.
ActMnMdn0 <- TotalStfilled %>% select (date, totalSteps)%>%
             summarise(mean = mean(totalSteps, na.rm = TRUE), 
                       median = median(totalSteps, na.rm = TRUE))
```

```{r}
# b3. comparison of dataset with NAs (first part of the assignment) and dataset with non-missing values:
ActMnMdn # dataset without NAs, imputed, Q3
ActMnMdn0 # dataset with NAs from Q2


# the the comparision indicates that both condition (date set with/without missing values) have the same mean and median values. The imputation doesnt result in considerable differences in mean/median values.
```

### Question 5. 
Are there differences in activity patterns between weekdays and weekends? 

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

##### 1. َCreate a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
# as_tibble(Activity1) # row dataset with NAs 
# ActfillingInt # filled dataset without NAs Dataset

ActWdays<- ActfillingInt

# added a column of days
ActWdays$wdays <- weekdays(as.POSIXlt(ActfillingInt$date))

# label the wdays based on Weekdays and Weekend
ActWdayType <- ActWdays%>% mutate(wday_type = ifelse(wdays %in% c("Saturday", "Sunday"),
                                                     "Weekend",
                                                     "Weekdays")) 
unique(ActWdayType$wday_type)
```

#### 2. Make a panel plot: 
Make a panel plot containing a time series plot (i.e. type = "l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r, warning=FALSE}
# created a dataset that is grouped by interval and mean of step is calculated for day types.
Act8Wdays <- ActWdayType %>% group_by(interval, wday_type) %>%
  summarise(meanSteps = mean(steps),
            .groups = "drop" )

library(lattice)
xyplot(meanSteps ~ interval | wday_type, 
       data = Act8Wdays, 
       type = "l", 
       layout = c(1,2), 
       col = "blue",
       xlab = "Total Steps/Interval", 
       ylab = "Average Steps",
       main = "Weekday vs Weekend Activity Patterns",
       par.settings = list(
         par.main.text = list(cex = 1)))
```





