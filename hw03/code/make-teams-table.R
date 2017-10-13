#Cleaning data
#-----------
#Description: Creating summary of efficiency, some plots
#Inputs: data files from Github
#Outputs: see Output folder

library(readr)
library(dplyr)
library(ggplot2)

setwd('/Users/Alina 1/stat133/stat133-hws-fall17')
dat <- read_csv('hw03/data/nba2017-stats.csv')

dat <- mutate(dat, missed_fg = field_goals_atts-field_goals_made, 
              missed_ft = points1_atts-points1_made, 
              points = points1_made+points2_made*2+points3_made*3, 
              rebounds = off_rebounds+def_rebounds, 
              efficiency = (points+rebounds+assists+steals+blocks-missed_fg-missed_ft-turnovers)/games_played)
sink('hw03/output/efficiency-summary.txt')
summary(dat$efficiency)
sink()
?join
dat1 <- read.csv('hw03/data/nba2017-roster.csv', stringsAsFactors = FALSE)
data <- full_join(dat,dat1)
data1 <- select(data,team, 
        experience, 
        salary, 
        points2_made, 
        points3_made, 
        points, 
        off_rebounds, 
        def_rebounds, 
        assists, 
        steals, 
        blocks, 
        turnovers, 
        fouls, 
        efficiency,
        points1_made)

teams <- data1%>%
  group_by(team) %>%
  mutate( salary = round ((salary/1000000), 2), 
         free_throws = points1_made) %>%
  summarise(experience=round(sum(experience), 2),
            salary=sum(salary), 
            points3=sum(points3_made),
            points2=sum(points2_made),
            free_throws=sum(points1_made),
            points=sum(points), 
            off_rebounds=sum(off_rebounds), 
            def_rebounds=sum(def_rebounds), 
            assists=sum(assists), 
            steals=sum(steals), 
            blocks=sum(blocks), 
            turnovers=sum(turnovers), 
            fouls=sum(fouls), 
            efficiency=sum(efficiency))
            
sink('hw03/output/teams-summary.txt')
summary(teams)
sink()

write_csv(teams,'hw03/data/nba2017-teams.csv')

#Some graphics

#1
pdf('hw03/images/teams_star_plot.pdf')
stars(teams[ ,-1], labels = teams$team)
dev.off()

#2
scatter <- ggplot(teams, aes(x = experience, y = salary)) +
  geom_point() +
  geom_text(label=teams$team, nudge_x = 5, nudge_y=5, alpha = 0.5)
scatter
ggsave('hw03/images/experience_salary.pdf')
