top10 Econ Programs
================
Superficial Intelligence (Nick, Jerry, Katherine, Cynthia)

Abstract
--------

In this project, we explore graduate admissions data from Grad Cafe. As all group members of Superficial Intelligence have experience with College and graduate school application, we became interested in quantitatively analyzing the graduate admission result. In this project, our main goal is to investigate the question of how do different variables in the dataset, such as GRE score and undergraduate GPA, relate to the admission decision. In the first part, we mainly investigate the distribution of single variables contained in this dataset. We will also extract useful information from the dataset by looking at the applications to different schools, application demographic, and overall application number. We first look at every single variable in the dataset and describe its pattern or trend if it has one. Then we look at the covariation of different variables. For this project deliverable 1, we look at four problems: The decision reported over time, the relationship between acceptance rate and season, the relationship between acceptance rate and student status, the selectivity of different graduate school programs. We use functions in R to plot the variables of interest. We found out as more students tend to report their decision to Grad Cafe these years, 60% of applicants are American students and 40% are international students. According to the figure, American students are more likely to get accepted compared to international students. In the future, we would like to use the logistic model to predict the probability of a student getting accepted by school programs.

Dataset Description
-------------------

For this project, we will use data on graduate admissions from Grad Cafe provided by Debarghya Das on GitHub. The graduate school admission result database includes admission results and detailed student test scores self-reported by prospective graduate students on <https://www.thegradcafe.com/>. The dataset contains 345,303 observations and 19 variables with a mix of continuous and categorical data. The dataset contains the following variables:

1.  **rowid (integer)** - An integer id of the row.

2.  **uni\_name (character)** - The name of the university.

3.  **major(character)** - The subject of the program self-reported by students.

4.  **degree (character)** - The type of degree program. The variable takes one of the following values: MS, MA, PhD, MFA, MBA, MEng, and Other.

5.  **season (character)** - The season of application. The first letter indicates whether the program starts from the Fall semester or Spring semester, and then the letter is followed by the last 2 digits of the year the program starts.

6.  **decision (character)** - The admission decision. It contains five categories - Accepted, Rejected, Wait-listed, Interview and Other.

7.  **decision\_method (character) ** - The method through which decision was communicated.

8.  **decision\_date (character)** - The date that the decision was communicated.

9.  **decision\_timestamp (integer)** - Timestamp of the decision.

10. **ugrad\_gpa (double)** - The respondent’s undergraduate GPA. The scale of the GPA varies because some students use a 10-point scale while others use a 4-point scale.

11. **gre\_verbal (double)** - GRE verbal score, which varies from 130 to 170 for the new GRE and from 200 to 800 for the old GRE.

12. **gre\_quant (double)** - GRE quantitative score, which varies from 130 to 170 for the new GRE and from 200 to 800 for the old GRE.

13. **gre\_writing (double)** - GRE writing score that ranges from 0 to 6.

14. **is\_new\_gre (logical)** - Whether or not the applicant took the new GRE.

15. **gre\_subject (double)** - GRE subject test score on a 200 to 990 score scale.

16. **status (character)** - Status of the candidate. Can be "International", "International with US Degree", "American" or "Other".

17. **post\_data (character)** - The date in which the observation was posted on grad cafe.

18. **post\_timestamp (integer)** - Timestamp of the post.

19. **comments (character)** - Applicants’ comments.

We decided to drop variables which either contain little information such as 'gre\_subject', which few candidates reported, and 'rowid' which is redundant, and variables which are not of interest to us, such as 'comments', 'decision\_method', 'post\_data', and 'post\_timestamp'.

Some problems that we may expect to encounter in the data are missing values, biased data due to self-reporting (it may be possible that positive results are more likely to be reported), and possibly fake data. In addition to this, the data will likely require some cleaning as user fill out forms and may be inconsistent (for example school name might be "Boston University"" or "BU"). Lastly, 'ugrad\_gpa' could be based on different scales, such as a 10 point scale that is sometimes used internationally.

Research Questions
------------------

Our main question of interest is "How do the different variables relate to admission decision?" We are interesting in understanding how the different factors such as GPA, GRE scores, the degree program you are applying to, or status affect whether you are ultimately chosen for admission. We also aim to answer several "sub-questions" such as:

-   How do admissions statistics differ across schools?

-   How do admissions differ between Boston University and "top tier" schools?

-   How have admissions statistics changed over time? (2015, 2016, 2017)?

-   Is there a relationship between acceptance rate and season? (Is it easier to get enrolled in Spring semester or Fall semester?)

-   Relationship between acceptance rate and student status (American vs International students; International students with a US degree vs those without)

-   Does applying earlier make a difference in getting into a school?

We would like to explore these questions to help all of us who are interested in graduate school to better understand the admission process.

Data Import & Cleaning
----------------------

We start by importanting the neccesary packages and the dataset.

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  2.0.1     ✔ dplyr   0.7.8
    ## ✔ tidyr   0.8.2     ✔ stringr 1.3.1
    ## ✔ readr   1.3.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(stringr)
#library(plotly)
(grad <- read_csv("data/grad.csv",
    col_types = cols_only( 
      uni_name=col_character(),
      major=col_character(),
      degree=col_character(),
      season=col_character(),
      decision=col_character(),
      decision_date=col_character(),
      decision_timestamp=col_double(),
      ugrad_gpa=col_double(),
      gre_verbal=col_double(),
      gre_quant=col_double(),
      gre_writing=col_double(),
      is_new_gre=col_logical(),
      status=col_character())))
```

    ## # A tibble: 345,303 x 13
    ##    uni_name major degree season decision decision_date decision_timest…
    ##    <chr>    <chr> <chr>  <chr>  <chr>    <chr>                    <dbl>
    ##  1 Univers… Ms. … MS     S16    Accepted (5, 11, 2015)       1446699600
    ##  2 Vanderb… Educ… MS     F16    Other    <NA>                        NA
    ##  3 Univers… Publ… MS     F16    Accepted (16, 11, 201…       1447650000
    ##  4 Tufts U… Comp… PhD    S16    Accepted (16, 11, 201…       1447650000
    ##  5 Univers… Theo… MS     F16    Accepted (16, 11, 201…       1447650000
    ##  6 Univers… Mast… MS     S16    Rejected (14, 11, 201…       1447477200
    ##  7 Univers… Publ… MS     F16    Accepted (12, 11, 201…       1447304400
    ##  8 Tufts U… MALD  MS     S16    Accepted (7, 11, 2015)       1446872400
    ##  9 New Yor… Fina… MS     S16    Accepted (15, 11, 201…       1447563600
    ## 10 Appalac… Comm… MS     S16    Accepted (13, 11, 201…       1447390800
    ## # … with 345,293 more rows, and 6 more variables: ugrad_gpa <dbl>,
    ## #   gre_verbal <dbl>, gre_quant <dbl>, gre_writing <dbl>,
    ## #   is_new_gre <lgl>, status <chr>

``` r
problems(grad)
```

    ## [1] row      col      expected actual  
    ## <0 rows> (or 0-length row.names)

As mentioned previously, we drop variables 'gre\_subject','rowid','comments', 'decision\_method', 'post\_data', and 'post\_timestamp'. Aside from this we have no problems regarding data import. While the dataset has some missing data, we keep all data for analyzing variation of single variables.

Variation of Single Variables:
------------------------------

First we plot counts for the most popular grad schools and programs are.

``` r
grad %>% group_by(uni_name) %>% count(uni_name) %>% arrange(desc(n))
```

    ## # A tibble: 4,535 x 2
    ## # Groups:   uni_name [4,535]
    ##    uni_name                                         n
    ##    <chr>                                        <int>
    ##  1 Columbia University                          10901
    ##  2 Stanford University                           9071
    ##  3 University Of California, Berkeley (UCB)      7796
    ##  4 University Of Michigan, Ann Arbor (UMich)     7585
    ##  5 Cornell University                            6983
    ##  6 University Of California, Los Angeles (UCLA)  6788
    ##  7 Harvard University                            6587
    ##  8 University Of Washington, Seattle (UW)        6507
    ##  9 New York University (NYU)                     6291
    ## 10 University Of Pennsylvania (UPenn)            6043
    ## # … with 4,525 more rows

``` r
grad %>% group_by(uni_name,major) %>% count(uni_name, major) %>% arrange(desc(n))
```

    ## # A tibble: 73,296 x 3
    ## # Groups:   uni_name, major [73,296]
    ##    uni_name                                      major                    n
    ##    <chr>                                         <chr>                <int>
    ##  1 Carnegie Mellon University (CMU)              Computer Science       776
    ##  2 Stanford University                           Computer Science       765
    ##  3 Georgia Institute Of Technology (GTech)       Computer Science       610
    ##  4 Columbia University                           Computer Science       585
    ##  5 University Of Illinois, Urbana-Champaign (UI… Computer Science       582
    ##  6 University Of Washington, Seattle (UW)        Computer Science       581
    ##  7 Stanford University                           Electrical Engineer…   559
    ##  8 Boston University (BU)                        Economics              553
    ##  9 University Of California, San Diego (UCSD)    Computer Science       530
    ## 10 Columbia University                           Economics              523
    ## # … with 73,286 more rows

``` r
school <- c("Harvard University", "Massachusetts Institute Of Technology (MIT)", "Princeton University", "Stanford University", "University Of California, Berkeley (UCB)", "Yale University", "Northwestern University", "University Of Chicago (UChicago)", "Columbia University", "University Of Pennsylvania (UPenn)")


(grad <- grad %>% filter(uni_name %in% school) %>% filter(major == "Economics"))
```

    ## # A tibble: 3,964 x 13
    ##    uni_name major degree season decision decision_date decision_timest…
    ##    <chr>    <chr> <chr>  <chr>  <chr>    <chr>                    <dbl>
    ##  1 Columbi… Econ… MS     F15    Accepted (18, 5, 2015)       1431921600
    ##  2 Columbi… Econ… MS     F15    Accepted (8, 5, 2015)        1431057600
    ##  3 Columbi… Econ… MS     F15    Rejected (23, 4, 2015)       1429761600
    ##  4 Columbi… Econ… MS     F15    Accepted (14, 5, 2015)       1431576000
    ##  5 Columbi… Econ… MS     F15    Rejected (29, 4, 2015)       1430280000
    ##  6 Columbi… Econ… MS     F15    Rejected (29, 4, 2015)       1430280000
    ##  7 Columbi… Econ… MS     F15    Rejected (29, 4, 2015)       1430280000
    ##  8 Columbi… Econ… MS     F15    Rejected (29, 4, 2015)       1430280000
    ##  9 Columbi… Econ… MS     F15    Rejected (29, 4, 2015)       1430280000
    ## 10 Columbi… Econ… MS     F15    Rejected (28, 4, 2015)       1430193600
    ## # … with 3,954 more rows, and 6 more variables: ugrad_gpa <dbl>,
    ## #   gre_verbal <dbl>, gre_quant <dbl>, gre_writing <dbl>,
    ## #   is_new_gre <lgl>, status <chr>

``` r
unique(grad$uni_name)
```

    ##  [1] "Columbia University"                        
    ##  [2] "Yale University"                            
    ##  [3] "University Of Pennsylvania (UPenn)"         
    ##  [4] "Northwestern University"                    
    ##  [5] "University Of California, Berkeley (UCB)"   
    ##  [6] "Harvard University"                         
    ##  [7] "University Of Chicago (UChicago)"           
    ##  [8] "Stanford University"                        
    ##  [9] "Princeton University"                       
    ## [10] "Massachusetts Institute Of Technology (MIT)"

Next, we look at the change in number of application over time.

``` r
grad$decision_date <- grad$decision_date %>%
  str_replace("\\(", "") %>%
  str_replace("\\)", "")


grad %>% 
  select(degree, decision_date) %>%
  filter(!is.na(decision_date)) %>%
  mutate(yr = str_match(decision_date, "...\\d$")) %>%
  group_by(degree, yr) %>%
  filter(degree == "MFA" | degree == "MS" | degree == "PhD") %>%
  filter(as.integer(yr) < 2016 ) %>% 
  filter(as.integer(yr) > 2005) %>%
  ggplot() +
  geom_bar(aes(yr, fill = degree), position = position_dodge2(preserve = "single")) +
  labs(x ="Year",
     y ="Count",
     title="Number of Application for each Degree Type by Year")
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-3-1.png)

The dataset has official data of report from 2006 to 2015. The application report of three degrees, MFA, MS, PhD increase each year until 2015. The overall shape has a plateauing trend.

Next, we look at the distribution of applications by season and the counts of each admission decision.

``` r
# bar chart for season
# bar chart includes both Spring semester and Fall semester
grad %>% 
  filter(!is.na(season)) %>%
  ggplot() +
  geom_bar(aes(x = season)) +
  labs(title = "Admission Season")
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# bar chart for Fall semester only
fall <- grad %>%
  filter(!is.na(season)) %>%
  group_by(fall = str_match(season, "^\\F..")) %>%
  filter(!is.na(fall)) %>%
  ggplot() +
  geom_bar(aes(x = season)) +
  labs(title = "Admission Season (Fall)")
# bar chart for Spring semester only
spring <- grad %>%
  filter(!is.na(season)) %>%
  group_by(fall = str_match(season, "^\\F..")) %>%
  filter(is.na(fall)) %>%
  ggplot() +
  geom_bar(aes(x = season)) +
  labs(title = "Admission Season (Spring)")

gridExtra::grid.arrange(fall, spring, ncol=2)
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# bar chart for decision
grad %>%
  filter(!is.na(decision)) %>%
  ggplot() +
  geom_bar(aes(x = decision)) +
  labs(title = "Admission Decision")
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-4-3.png)

We see that the total number of application are signicantly higher for the fall semester than the Spring semester. Focusing only on fall semesters, graduate school enrollment clearly has shown a positive trend over the years. Spring enrollment does not show an explicit pattern. When looking at the distribution of decisions, the majority of candidates either recieve of report mainly acceptances and rejections, while a few candiates recieve other forms of responses such as waitlist, interview, or "other."

Next, we plot the distribution of GRE test scores, and GPA. Because these is a variable "is\_new\_gre", which distinguished between old and new GRE, we filter for only new GRE scores, as the majority of observations report new GRE scores.

``` r
# GRE Verbal
grad %>% select(gre_verbal ,is_new_gre) %>% 
filter(is_new_gre == TRUE & is.na(gre_verbal)!= TRUE ) %>% ggplot + geom_histogram(aes(gre_verbal)) + 
labs(x ="GRE Verbal Score",
     y ="Count",
     title="Frequencies of GRE Verbal Scores")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# GRE quant
grad %>% select(gre_quant ,is_new_gre) %>% 
  filter(is_new_gre == TRUE & is.na(gre_quant)!= TRUE ) %>% ggplot + geom_histogram(aes(gre_quant)) + 
labs(x ="GRE Quant Score",
     y ="Count",
     title="Frequencies of GRE Quant Scores")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
# GRE writing
grad %>% select(gre_writing ,is_new_gre) %>% 
  filter(is_new_gre == TRUE & is.na(gre_writing)!= TRUE) %>% ggplot + geom_histogram(aes(gre_writing)) + 
labs(x ="GRE Writing Score",
     y ="Count",
     title="Frequencies of GRE Writing Scores")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-5-3.png)

We see from the above histograms that GRE verbal scores range from 130 to 170 with a bell shape. Most of them concentrate 155 - 165. GRE quant score range from 130 to 170 with step like shape. Scores tend to concentrate 160 - 170. GRE writing scores range from 2 to 6 with a bell like shape. Most people get a score of 4.

``` r
grad %>% filter(!is.na(ugrad_gpa) & ugrad_gpa < 4.0) %>% 
  ggplot(aes(ugrad_gpa)) + geom_histogram(bins = 40) + labs(titles = "GPA Distribution")
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-6-1.png)

We see that the distribution of GPAs for the observations tend to be left skewed, with the majority of candidates having more than 3.6 GPA. This is accepted as grad programs tend to look at GPA as a major factor, and students who aim to attend a grad school would likely have higher GPAs.

Lastly, we look at the distribution of student status (internation, US, international with US degree, etc)

``` r
grad %>% 
  filter(!is.na(status)) %>% 
  mutate(count = n()) %>% 
  ggplot(aes(x = status)) + geom_bar() + 
  labs(titles = "Immigration Status") 
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-7-1.png)

From the chart above, we see that the majority of students applying are American. In Immigration Status, around 60% of applicants are American and the rest of them are international students. We can tell that a big amount of graduate or Ph.D. students are coming from an international background.

Covariation Between Multiple Variables
--------------------------------------

One covariation of interest is the influence of student status (internation, US, etc.) vs admission decision.

``` r
# student identity vs acceptance rate
# table for student status vs decision
(grid <- grad %>%
  filter(!is.na(status), !is.na(decision)) %>%
  group_by(status, decision) %>%
  summarise(count = n()) %>%
  spread(key = decision, value = count))
```

    ## # A tibble: 4 x 6
    ## # Groups:   status [4]
    ##   status                    Accepted Interview Other Rejected `Wait listed`
    ##   <chr>                        <int>     <int> <int>    <int>         <int>
    ## 1 American                       390         1    26      955           114
    ## 2 International                  456         2    37     1065           129
    ## 3 International with US De…      178         2    14      445            37
    ## 4 Other                            7        NA    26       10            NA

``` r
# bar chart 
grad %>%
  filter(!is.na(decision), !is.na(status)) %>%
  ggplot() +
  geom_bar(aes(status, fill = decision), position = "dodge") +
  labs(title = "Admission Decision vs Status")
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
# acceptance rate based on student status
grid$sum = rowSums(grid[, c("Accepted", "Interview", "Other", "Rejected", "Wait listed")])
grid %>%
  gather('Accepted', 'Interview', 'Other', 'Rejected', 'Wait listed', 
         key = "decision", value = "cases") %>%
  mutate(prop = cases/sum) %>%
  filter(status != "Other") %>%
  ggplot(aes(group = decision, status, prop, color = decision)) + 
  geom_line()
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-8-2.png)

From the charts above, it seems that US based students tend to have higher acceptance rates than international students, and internation with US degree students. The bar chart shows that, for American students, the number of getting accepted is higher than the number of getting rejected. However, for international students and international students with US degree, the number of acceptance is lower than the number of rejection. To further investigate if international students are treated differently, we calculate the decision rate. For each status category, we divide the total number of each admission decision by total number of students to get the decision rate. From the plot we can tell that the proportion of getting accepted is higher for American students than international students, and the proportion of getting rejected is higher for international students with US degree.

Another covariation of interest is the relationship between GPA and GREE scores. For this we summed GRE verbal and GRE quant to get the full GRE score, and created a scatter plot against GPA. We filtered GPA to be less than 4, as GPA of different scales are not comparable.

``` r
grad %>% 
  filter(!is.na(ugrad_gpa|gre_verbal|gre_quant)& ugrad_gpa < 4 & ugrad_gpa >1, is_new_gre == TRUE) %>%
  mutate(GRE_Total = gre_verbal + gre_quant) %>% 
  group_by(uni_name) %>% 
  mutate(mean_gpa = mean(ugrad_gpa), mean_GRE = mean(GRE_Total)) %>% 
  ungroup() %>%
  ggplot(aes(x = mean_GRE, y = mean_gpa)) + geom_point(aes(color = uni_name, alpha = 0.001)) +
  labs(titles = "Relationship between mean GPA and mean GRE Score",
       y = "Mean GPA",
       x = "Mean GRE Score")
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
grad %>% 
  filter(!is.na(ugrad_gpa|gre_verbal|gre_quant)& ugrad_gpa < 4 & ugrad_gpa >1, is_new_gre == TRUE) %>%
  mutate(GRE_Total = gre_verbal + gre_quant) %>% 
  group_by(uni_name) %>% 
  mutate(mean_gpa = mean(ugrad_gpa), mean_GRE = mean(GRE_Total)) %>% 
  ungroup() %>%
  ggplot(aes(GRE_Total, ugrad_gpa)) +
  geom_point(aes(color = "RED", alpha = 0.001)) +
  labs(titles = "Relationship between GPA and GRE Score",
       y = "GPA",
       x = "GRE Score")
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-9-2.png)

From the plot above the relationship between GPA and GRE seems to be positively correlated but is not as strong of a relationship as we expected. Most GPAs tend to be on the higher range: people densely fall into the range between 3.5 and 3.75; GRE scores seem to be more variable across application: scores for all applicants concentrate in the range between 300 and 325 with more outliers.

Lastly, to measure the correlation across all continuous variables, we create a scatterplot matrix, and correlation matrix.

``` r
g <- grad[complete.cases(grad),] %>% mutate(acceptance = decision == "Accepted") %>% filter(ugrad_gpa<=4,is_new_gre == TRUE) %>% select(ugrad_gpa, gre_verbal,gre_quant,gre_writing,acceptance) %>% pairs()
```

![](top10_Econ_files/figure-markdown_github/unnamed-chunk-10-1.png)

This plot is somewhat unclear due to the verse dense concentration of the datapoints. In the next step, we will likely use regression to model the probability of acceptance based on the different covariates.

Discussion:
-----------

From this exploratory data analysis, we confirm many of the hypotheses that we had going into this project. For example, we confirmed our hypotheses that more students apply in the fall semester than the spring semester and that the majority of students applying (or reported) are American. We confirmed relationships between variables such as GPA and GRE scores. We learned several things as well. For example, we learned the distribution of GPA is left skewed, and GRE scores have an abnormal distribution, with several "spikes" among certain scores. We were surprised to see that American students tended to have higher rates of acceptance than international students.

While this is a very interesting and robust dataset to analyze, there are also several problems we encountered. First, the dataset is not very clean, as it is self-reported. For example, the names of Universities and Majors are not always consistent. For example, some students may write "Boston University (BU)" while others write the name of the specific college at BU such as "Boston University - Metropolitan College." We also noticed that scales of scores and GPA are not always consistent. For example, GPA is most often reported on a 4.0 scale, however, some responses included other scales such as 10 point scale. These will all be problems that we have to work around when going into modeling.
