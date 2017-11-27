#-------
#Alina Skripets
#Functions
#--------
library(dplyr)
library(stringr)

#remove missing
#Description: removes missing values
#Input:a numeric vector
#Output: vector with no missing values
remove_missing <- function(vec) {
output <- numeric()
for (i in 1:length(vec))
  {if (is.na(vec[i])) {next} else {output <- c(output, vec[i])}
}
return(output)
}
#Check
vec <- c(NA,6,7,3,7)
remove_missing(vec)

#get_minimum
#Description: returns minimum
#Input: numeric vector
#Output: minimum
get_minimum <- function(vec, na.rm = TRUE) {
  stopifnot(is.numeric(vec))
if (na.rm == TRUE) {vec <- remove_missing(vec) 
vec <- sort(vec)
return(vec[1]) } else {
  vec <- sort(vec)
  return(vec[1]) 
}
}

#Check
a <- c(1, 4, 7, NA, 10)
get_minimum(a, na.rm = FALSE)


#get_maximum
#Description: returns max of a vector
#Input: numeric vector
#Output: maximum
get_maximum <- function(vec, na.rm = TRUE) {
  stopifnot(is.numeric(vec))
  if (na.rm == TRUE) {vec <- remove_missing(vec) 
  vec <- sort(vec, decreasing = TRUE)
  return(vec[1]) } else {
    vec <- sort(vec, decreasing = TRUE)
    return(vec[1]) 
  }
}

#Check
a <- c(4, 7, NA, 10)
get_maximum(a, na.rm = TRUE)

#get_range
#Description: returns range of a vector
#Input:numeric vector
#Output:range of a vector
get_range <- function(vec, na.rm=TRUE){
  stopifnot(is.numeric(vec))
  if (na.rm == TRUE) {vec <- remove_missing(vec) 
  range1 <- get_maximum(vec) - get_minimum(vec)
  return(range1) } else {
    range1 <- get_maximum(vec) - get_minimum(vec)
    return(range1)
  }
}

#Check
a <- c(1, 4, 7, NA, 10)
get_range(a, na.rm = TRUE)

#get_percentile10
#Description: returns 10th percentile of a vector
#Input: numeric vector
#Output: 10th percentile
get_percentile10 <- function(vec, na.rm=TRUE){
  
  stopifnot(is.numeric(vec))
  if (na.rm == TRUE) {vec <- remove_missing(vec) 
  
  perc10 <- unname(quantile(vec, probs = 0.1))
  
  return(perc10) } else {
    
  perc10 <- unname(quantile(vec, probs = 0.1))
    
    return(perc10)
  }
}

#Check
a <- c(1, 4, 7, NA, 10)
get_percentile10(a, na.rm = TRUE)

#get_percentile90
#Description: retuns 90th percentile of a vector
#Input: numeric vector
#Output: 90th percentile
get_percentile90 <- function(vec, na.rm=TRUE){
  stopifnot(is.numeric(vec))
  if (na.rm == TRUE) {vec <- remove_missing(vec) 
  
  perc90 <- unname(quantile(vec, probs = 0.9))
  
  return(perc90) } else {
    
    perc90 <- unname(quantile(vec, probs = 0.9))
    
    return(perc90)
  }
}
#Check
a <- c(1, 4, 7, NA, 10)
get_percentile90(a, na.rm = TRUE)

#get_median
#Description: returns a median of a vector
#Input: numeric vector
#Output: median
get_median <- function(vec, na.rm=TRUE){
  stopifnot(is.numeric(vec))
  if (na.rm == TRUE) {vec <- remove_missing(vec) 
  
  medi <- unname(quantile(vec, probs = 0.5))
  
  return(medi) } else {
    
    medi <- unname(quantile(vec, probs = 0.5))
    
    return(medi)
  }
}
#Check
a <- c(1, 4, 7, NA, 10)
get_median(a, na.rm = TRUE)

#get_average
#Description: returns mean of a vector
#Input: numeric vector
#Output: average of a vector
get_average <- function(vec, na.rm=TRUE){
  stopifnot(is.numeric(vec))
  if (na.rm == TRUE) {vec <- remove_missing(vec) 
  
  sum <- 0
  for (i in 1:length(vec)) {sum <- sum+vec[i]}
  aver <- 1/(length(vec))*sum
  
  return(aver) } else {
    
    sum <- 0
    for (i in 1:length(vec)) {sum <- sum+vec[i]}
    aver <- 1/(length(vec))*sum
    
    return(aver)
  }
  }

#Check
a <- c(1, 4, 7, NA, 10)
get_average(a, na.rm = TRUE)



#get_stdev
#Description: returns sd of a vector
#Input: numeric vector
#Output: standard deviation
get_stdev <- function(vec, na.rm=TRUE){
  stopifnot(is.numeric(vec))
  if (na.rm == TRUE) {vec <- remove_missing(vec) 
  sum <- 0
  for (i in 1:length(vec)) {
  sum <- sum+((vec[i]-get_average(vec, na.rm = TRUE))^2)                                      }
  sd <- sqrt(1/(length(vec)-1)*sum)
  return(sd) 
   } else {
    sum <- 0
    for (i in 1:length(vec)) {sum <- sum+print((vec[1]-get_average(vec, na.rm = FALSE))^2)                                     }
    sd <- sqrt(1/(length(vec)-1)*sum)
    return(sd)
    }
  }

#Check
a <- c(1, 4, 7, NA, 10)
get_stdev(a, na.rm = TRUE)

#get_quartile1
#Description: returns 25th percentile of the vector
#Input: numeric vector
#Output: 1st quartile
get_quartile1 <- function(vec, na.rm=TRUE){
  stopifnot(is.numeric(vec))
  if (na.rm == TRUE) {vec <- remove_missing(vec) 
  
  q1 <- unname(quantile(vec, probs = 0.25))
  
  return(q1) } else {
    
    q1 <- unname(quantile(vec, probs = 0.25))
    
    return(q1)
  }
}
#Check
a <- c(1, 4, 7, NA, 10)
get_quartile1(a, na.rm = TRUE)

#get_quartile3
#Description: returns 25th percentile of the vector
#Input: numeric vector
#Output: 3d quartile
get_quartile3 <- function(vec, na.rm=TRUE){
  stopifnot(is.numeric(vec))
  if (na.rm == TRUE) {vec <- remove_missing(vec) 
  
  q3 <- unname(quantile(vec, probs = 0.75))
  
  return(q3) } else {
    
    q3 <- unname(quantile(vec, probs = 0.75))
    
    return(q3)
  }
}
#Check
a <- c(1, 4, 7, NA, 10)
get_quartile3(a, na.rm = TRUE)

#count_missing
#Description: counts missing values in a vector
#Input: numeric vector
#Output: number of missing values
count_missing <- function(vec, na.rm=TRUE){
  stopifnot(is.numeric(vec))
count <- sum(is.na(vec))
return(count)
}
#Check
a <- c(1, 4, 7, NA, 10, NA, NA)
count_missing(a, na.rm = TRUE)

#summary_stats
#Description: returns summary of all the stats for the vector
#Input: numeric vector
#Output: list of stats
summary_stats <- function(vec, na.rm = TRUE)
{ mlist <- list(
  get_minimum(vec, na.rm),
  get_percentile10(vec, na.rm),
  get_quartile1(vec, na.rm),
  get_median(vec, na.rm),
  get_average(vec, na.rm),
  get_quartile3(vec, na.rm),
  get_percentile90(vec, na.rm),
  get_maximum(vec, na.rm),
  get_range(vec, na.rm),
  get_stdev(vec, na.rm),
  count_missing(vec)
  )
  names(mlist) <- c("minimum", 
                    "percent10", 
                    "quartile1", 
                    "median", 
                    "mean", 
                    "quartile3",
                    "percent90", 
                    "maximum", 
                    "range", 
                    "stdev", 
                    "missing"
    )
  return(mlist)
}

#Check
a <- c(1, 4, 7, NA, 10)
stats <- summary_stats(a, na.rm = TRUE)
stats


#print_stats
#Description: print stats in a neat way
#Input: numeric vector
#Output: neat print of stats
print_stats <- function(vec, na.rm = TRUE)
{
  for (i in 1:11){ 
    cat(noquote(str_pad( 
      c(names(summary_stats(vec)[i]),":", sprintf('%.4f', summary_stats(vec)[[i]])), 9, "right")), "\n")
     }
}



#Check
a <- c(1, 4, 7, NA, 10)
print_stats(a, na.rm = TRUE)

#rescale100
#Description: rescales a grade according to min and max
#Input: numeric value, min and max 
#Output: rescaled value
rescale100 <- function(x, xmin, xmax){
  z <- 100*(x-xmin)/(xmax-xmin)
  return(z)
}

#Check
b <- c(18, 15, 16, 4, 17, 9)
rescale100(b, xmin = 0, xmax = 20)

#drop_lowest
#Description: drops one lowest value
#Input: numeric vector
#Output: numeric vector of length less 1
drop_lowest <- function(vec){
  #a <- get_minimum(vec)
  #output <- vec[!vec%in%a]
  output <- sort(vec)
  return(output[2:(length(vec))])
}
#Check
b <- c(10, 10, 8.5, 4, 7, 9, 1, 0, 0)
drop_lowest(b)

#score_homework
#Description: gets average of the vector less the lowest value
#Input: numeric vector
#Output: numeric value
score_homework <- function(vec, drop=TRUE){

    if (drop == TRUE) { drop_lowest(vec)
    vec <- drop_lowest(vec) 
    output <- get_average(vec)
    
    return(output) } else {
      
      output <- get_average(vec)
      
      return(output)
    }
  }
#Check
hws <- c(100, 80, 30, 70, 75, 85)
score_homework(hws, drop = TRUE)
score_homework(hws, drop = FALSE)

#score_quiz
#Description: gets average of the vector less the lowest value
#Input: numeric vector
#Output: numeric value
score_quiz <- function(quiz, drop=TRUE){
  
  if (drop == TRUE) { drop_lowest(quiz)
    quiz <- drop_lowest(quiz) 
    output <- get_average(quiz)
    
    return(output) } else {
      
      output <- get_average(quiz)
      
      return(output)
    }
}

#Check
quizzes <- c(100, 80, 70, 0)
score_quiz(quizzes, drop = TRUE)
score_quiz(quizzes, drop = FALSE)

#score_lab
#Description: scores attendance accoring to a scale given
#Input: numeric vector
#Output: numeric value
score_lab <- function(att){
  switch(att,
         0,0,0,0,0,0,20,40,60,80,100,100)
}
#Check
score_lab(12)
score_lab(10)
score_lab(6)

