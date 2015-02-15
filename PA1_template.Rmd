Reproducible Research: Peer Assessment 1 
===========================================================================================
###Loading and preprocessing the data

```{r Load data,highlight=TRUE,echo=TRUE,message=FALSE,warning=FALSE}
# Please replace the "Username" with local username.
# The zip file containing the data needs to be downloaded at this location
setwd("C:/Users/z075313/Downloads")
unzip("repdata2Fdata2Factivity.zip","./")
readfile<- read.csv("activity.csv",header = TRUE,sep=",",stringsAsFactors=FALSE)
str(readfile)
#Date is not in proper format, it's coming in chr  "2012-10-01" format. Lets convert to Date form
readfile$date<-as.Date(readfile$date)
str(readfile)
 #Now its in correct form. Lets see how the data looks

head(readfile)
# We have NA's, lets check total number of NA's
Totalmissing<-sum(is.na(readfile$steps))
print(Totalmissing)
```

### What is mean total number of steps taken per day?
We need to remove na's from the data
```{r,echo=TRUE,highlight=TRUE,message=FALSE,warning=FALSE}
#Remove NA's
nona<-na.omit(readfile)
head(nona)
#Let proceess the steps per day. 
daywise<-tapply(nona$steps, nona$date, sum)
# Now this contains per day number of steps taken, lets plot the histogram and see
hist(daywise,10, main = "Total number of steps taken per day")
mean(daywise)
median(daywise)
```

### What is the average daily activity pattern?
```{r,echo=TRUE,highlight=TRUE,message=FALSE,warning=FALSE}
# To get the avg daily pattern we need see the pattern throughout the day at every 5mins interval.
# For every 5mins interval we will take the avg of all the days
activitypat<-tapply(nona$steps,nona$interval,mean)
plot(y = activitypat, x = names(activitypat), type = "l", xlab = "5-Minute-Interval", 
     main = "Daily Activity Pattern", ylab = "Average number of steps")
#Top avg step
activitypat[activitypat==max(activitypat)]
```

### Imputing missing values
```{r,echo=TRUE,highlight=TRUE,message=FALSE,warning=FALSE}
# We already checked the total number of NAs, lets call that again
Totalmissing<-sum(is.na(readfile$steps))
print(Totalmissing)
totalcount<-sum(!is.na(readfile$steps))
print(totalcount)
# so we have ~15% of missing value affecting the data.
# To fix this bias data we will take the mean of 5mins interval 
newreadfile <- readfile 
newreadfile[which(is.na(newreadfile$steps)),1]<-
        activitypat[as.character(newreadfile[which(is.na(newreadfile$steps)),3])]
# Check if any NAs exists

sum(is.na(newreadfile))
# So data is out of the NAs

# Lets compare the old histogram and the new one
activitypat_new<-tapply(newreadfile$steps, newreadfile$date, sum)

hist(daywise,10, main = "Total number of steps taken per day",ylim =c(0, 25))
hist(activitypat_new,10, main = "Total number of steps taken per day  
     (missing values replaced with mean of interval)", xlab = "Steps",
     ylim =c(0, 25))
# Calculating mean and median
mean(activitypat_new)
median(activitypat_new)
# Both are same

```


### Are there differences in activity patterns between weekdays and weekends?
```{r,echo=TRUE,highlight=TRUE,message=FALSE,warning=FALSE}
# we will create variable for weekdays and weekend

newreadfile$wd<-weekdays(newreadfile$date)
newreadfile$fwd<- as.factor(c("weekend", "weekday")) 
newreadfile[newreadfile$wd == "Sunday" | newreadfile$wd == "Saturday" ,5]<- factor("weekend") 
newreadfile[!(newreadfile$wd == "Sunday" | newreadfile$wd == "Saturday"),5 ]<- factor("weekday")

# We will create arrays for weekdays and weekend

newreadfile_we <- subset(newreadfile, fwd == "weekend")
newreadfile_wd <- subset(newreadfile, fwd == "weekday")
activity_we<-tapply(newreadfile_we$steps, newreadfile_we$interval, mean)
activity_wd<-tapply(newreadfile_wd $steps, newreadfile_wd $interval, mean)

plot(y = activity_wd, x = names(activity_wd), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekdays", ylab = "Average number of steps", 
     ylim =c(0, 250))
plot(y = activity_we, x = names(activity_we), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekends", ylab = "Average number of steps", 
     ylim =c(0, 250))
```
