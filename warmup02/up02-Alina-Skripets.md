WarmUp2
================
Alina Scripts
September 11, 2017

Lets download a file to my computer's working directory first and upload it to R working space.

``` r
rm(list=ls())
github <- 'https://github.com/ucb-stat133/stat133-hws-fall17/'
repo <- 'raw/master/warmup02/data/nba2017-salary-points.RData'

download.file(url = paste0(github, repo), 
destfile = "nba2017-salary-points.RData")
```

``` r
rm(list=ls())
load("nba2017-salary-points.RData")
```

Now we list all the variables that we have in the data package. We notice that because I am not very good at R, I have two extra objects: github and repo.Or not, I fixed it using rm function.

``` r
# list the available objects
ls()
```

    ## [1] "experience" "player"     "points"     "points1"    "points2"   
    ## [6] "points3"    "position"   "salary"     "team"

Now we hapazardly try to find out that type of salary is ' double ', whether or not team is a vector - FALSE, whether or not position object is a list - FALSE and what is the class of player object, which is' character '

We successfully used indented code! But I still do't understand what the professor meant by finding out *flavor*.

Now we check that all the objects are the same length:

``` r
length(salary)
```

    ## [1] 441

``` r
length(team)
```

    ## [1] 441

``` r
length(position)
```

    ## [1] 441

``` r
length(player)
```

    ## [1] 441

``` r
length(points)
```

    ## [1] 441

``` r
length(experience)
```

    ## [1] 441

``` r
length(points1)
```

    ## [1] 441

``` r
length(points2)
```

    ## [1] 441

``` r
length(points3)
```

    ## [1] 441

They are the same length! On to the next part.

Quantitative variable: *Salary*
===============================

``` r
summary(salary)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##     5145  1286160  3500000  6187014  9250000 30963450

``` r
mean(salary)
```

    ## [1] 6187014

``` r
sd(salary)
```

    ## [1] 6571890

``` r
min(salary)
```

    ## [1] 5145

``` r
max(salary)
```

    ## [1] 30963450

``` r
median(salary)
```

    ## [1] 3500000

``` r
quantile(salary, probs = seq (0, 1, 0.25))
```

    ##       0%      25%      50%      75%     100% 
    ##     5145  1286160  3500000  9250000 30963450

I can find all the statistics above, no problem. But how do i find typical value/ mode? I have no idea.

``` r
?tabulate
getmode <- function(salary) {
  uniqv <- unique(salary)
  uniqv[which.max(tabulate(match(salary,uniqv)))]
}
getmode(salary)
```

    ## [1] 543471

Wait, I did it! Now I can also find a spread by substracting min from max.

``` r
spread= max(salary)-min(salary)
spread
```

    ## [1] 30958305

Oooo, what do we have here.

``` r
hist(salary)
```

![](up02-Alina-Skripets_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

``` r
boxplot(salary)
```

![](up02-Alina-Skripets_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-2.png) /

/**What is the overall shape?** This histogram is scewed to the right with th bulk of salaries in the first bin.

**What is the typical center?** Typical center is at the left.

**Overall range, along with an interval of typical measurements.** Graphs have a huge range and typical interval is small because we have lots of data.

**Try to explain why your observations are important or interesting.** Observation is interesting because it proves that while the mean of the distribution is pretty high, it doesn't mean that players earn a lot on average. Median here is a better description of data.

How do I creat a density polygon? No idea :(
============================================

``` r
??density
y <- c(density(salary))

#cdplot(salary)
```

Catigorical variable: *Team*
============================

Is team variable a factor? TRUE Lets also find out how many variable are in each category aka frequency of each team.

``` r
teamfreq <- table(team)
teamfreq
```

    ## team
    ## ATL BOS BRK CHI CHO CLE DAL DEN DET GSW HOU IND LAC LAL MEM MIA MIL MIN 
    ##  14  15  15  15  15  15  15  15  15  15  14  14  15  15  15  14  14  14 
    ## NOP NYK OKC ORL PHI PHO POR SAC SAS TOR UTA WAS 
    ##  14  15  15  15  15  15  14  15  15  15  15  14

Based on this, we can calculate proportions or relative frequencies of each team.

``` r
prop.table(teamfreq)
```

    ## team
    ##        ATL        BOS        BRK        CHI        CHO        CLE 
    ## 0.03174603 0.03401361 0.03401361 0.03401361 0.03401361 0.03401361 
    ##        DAL        DEN        DET        GSW        HOU        IND 
    ## 0.03401361 0.03401361 0.03401361 0.03401361 0.03174603 0.03174603 
    ##        LAC        LAL        MEM        MIA        MIL        MIN 
    ## 0.03401361 0.03401361 0.03401361 0.03174603 0.03174603 0.03174603 
    ##        NOP        NYK        OKC        ORL        PHI        PHO 
    ## 0.03174603 0.03401361 0.03401361 0.03401361 0.03401361 0.03401361 
    ##        POR        SAC        SAS        TOR        UTA        WAS 
    ## 0.03174603 0.03401361 0.03401361 0.03401361 0.03401361 0.03174603

``` r
barplot(prop.table(teamfreq))
```

![](up02-Alina-Skripets_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png) /

**What is the overall shape?** Pretty much flat.

**What is the typical center?** Each team is approximately the same size.

**Overall range, along with an interval of typical measurements.** 14 or 15 players in eah team.

**Try to explain why your observations are important or interesting.** Clearly, game rules and team size regulations dictated the shape of the barplot.

Reflections
===========

<table>
<colgroup>
<col width="50%" />
<col width="50%" />
</colgroup>
<thead>
<tr class="header">
<th>Question</th>
<th>Answer</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>What things were hard, even though you saw them in class?</td>
<td>Finding a mode.</td>
</tr>
<tr class="even">
<td>What was easy(-ish) even though we haven't done it in class?</td>
<td>It was easy to figure out indented code.</td>
</tr>
<tr class="odd">
<td>What type of &quot;errors&quot; you struggled with (if any)?</td>
<td>I still can't figure out density polygon.</td>
</tr>
<tr class="even">
<td>What are the parts you are not fully understanding?</td>
<td>Falvor of an object and density polygon creation. Creating my own function for mode was hard too.</td>
</tr>
<tr class="odd">
<td>What was the most time consuming part?</td>
<td>Comments.</td>
</tr>
<tr class="even">
<td>Did you collaborate with other students? If so, with who? In what manner?</td>
<td>No, Piazza was unresponsive.</td>
</tr>
<tr class="odd">
<td>Was there any frustrating issue? (e.g. RStudio cryptic error, one or more package not playing nice)</td>
<td>Could get the ls() function to not list github and repo as separate elements.</td>
</tr>
</tbody>
</table>

For people who want to take things further.
-------------------------------------------

How to use the R package "xtable" to produce nicely formatted tables.

``` r
#install.packages("xtable")
```

``` r
library("xtable")
```

``` r
?par
plot(points,salary,type="l", col= "light blue",lwd = 5)
```

![](up02-Alina-Skripets_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)
