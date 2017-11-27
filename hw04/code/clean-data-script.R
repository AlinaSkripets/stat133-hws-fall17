#-----------
#Alina Skripets
#Clean Data Script
#-----------

source("./functions.R")
dat <- read.csv('../data/rawdata/rawscores.csv', stringsAsFactors = FALSE)
#Get stats
sink('../output/summary-rawscores.txt')
str(dat)
for (i in 1:334){
print(paste('For colomne', i, 'the summary is:'))
vec <- summary_stats(as.numeric(unname(dat[i, ]), TRUE))
print(vec)
print_stats(as.numeric(unname(dat[i, ]), TRUE))
print(noquote('   '))}
sink()
#Remove NA
for (i in 1:334){
  for (j in 1:16){
    if(is.na(dat[i,j])){dat[i,j] <- 0}
  }
}
#Rescale Quiz
dat$QZ1 <- rescale100(dat$QZ1, xmin <- 0, xmax <- 12) 
dat$QZ2 <- rescale100(dat$QZ2, 0, 18)
dat$QZ3 <- rescale100(dat$QZ3, 0, 20)
dat$QZ4 <- rescale100(dat$QZ4, 0, 20)
#Rescale Exams
dat$Test1 <- rescale100(dat$EX1, 0, 80)
dat$Test2 <- rescale100(dat$EX2, 0, 90)

#Add HW
for (i in 1:334){
  vec <- unlist(unname(dat[i, 1:9]))
  dat$Homework[i] <- score_homework(unlist(vec)) }
#Alt. way
 #   vec <- numeric(8)
  #  vec <- unlist(unname(dat[i, 1:9]))
   # vec <- drop_lowest(unname(dat[i, 1:9]))
    #dat$Homework[i] <- get_average(unlist(vec))
#}

#Add Quiz
for (i in 1:334){
  vec <- unlist(unname(dat[i, 11:14]))
  dat$Quiz[i] <- score_quiz(unlist(vec)) }
#Alt. way
#for (i in 1:334){
 # vec <- numeric(8)
  #vec <- unlist(unname(dat[i, 11:14]))
  #vec <- drop_lowest(unname(dat[i, 11:14]))
  #dat$Quiz[i] <- get_average(unlist(vec))
#}

#Add Lab
for (i in 1:334){
  vec <- dat$ATT[i]
  dat$Lab[i] <- score_lab(unlist(vec))
}

#Add Overall
for (i in 1:334){
dat$Overall[i] <- 0.1*dat$Lab[i]+0.3*dat$Homework[i]+0.15*dat$Quiz[i]+0.2*dat$Test1[i]+0.25*dat$Test2[i]
}

#Grade
for (i in 1:334){
if (dat$Overall[i] <= 100 & dat$Overall[i] >= 95){dat$Grade[i]='A+'}
else if (dat$Overall[i] < 95 &dat$Overall[i] >=90){dat$Grade[i]='A'}
else if (dat$Overall[i] < 90 &dat$Overall[i] >=88){dat$Grade[i]='A-'}
else if (dat$Overall[i] < 88 &dat$Overall[i] >=86){dat$Grade[i]='B+'}
else if (dat$Overall[i] < 86 &dat$Overall[i] >=82){dat$Grade[i]='B'}
else if (dat$Overall[i] < 82 &dat$Overall[i] >=79.5){dat$Grade[i]='B-'}
else if (dat$Overall[i] < 79.5 &dat$Overall[i] >=77.5){dat$Grade[i]='C+'}
else if (dat$Overall[i] < 77.5 &dat$Overall[i] >=70){dat$Grade[i]='C'}
else if (dat$Overall[i] < 70 &dat$Overall[i] >=60){dat$Grade[i]='C-'}
else if (dat$Overall[i] < 60 &dat$Overall[i] >=50){dat$Grade[i]='D'}
else if (dat$Overall[i] < 50){dat$Grade[i]='F'}
  }


#Get more stats
for (i in 17:22){
sink(paste('../output/', names(dat[i]), ' - stats.txt'), type = 'output')
print_stats(unlist(unname(summary_stats(unlist(unname(dat[i]))))))
sink()
}

#Even more sinking
sink('../output/summary-cleanscores.txt')
str(dat)
sink()

#Exporting
write.csv(dat, '../data/cleandata/cleanscores.csv')
