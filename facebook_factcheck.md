facebook\_factcheck
================

R Markdown
----------

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

need to write a proposal covering these questions: What is the problem you want to solve?

Who is your client and why do they care about this problem? In other words, what will your client DO or DECIDE based on your analysis that they wouldn’t have otherwise?

What data are you going to use for this? How will you acquire this data?

In brief, outline your approach to solving this problem (knowing that this might change later).

What are your deliverables? Typically, this would include code, along with a paper and/or a slide deck.

DATA WRANGLING:

Clean up your column names to be simple, short and descriptive For each column:

Check for missing values and decide what you want to do about them. Make sure the values in each column make sense. If you find values that don't, decide what you want to do about those.

Look for outliers (values that are too small or too large). Do they make sense? Do you want to keep them in the data set?

Discuss with your mentor about other data wrangling steps you might need to perform for your specific problem and implement those. Save your cleaned up and transformed data set.

Possible questions:

How do left, mainstream, and right categories of Facebook pages differ in the stories they share?

Which types of stories receive the most engagement from their Facebook followers? Are videos or links more effective for engagement?

Can you replicate BuzzFeed’s findings that “the least accurate pages generated some of the highest numbers of shares, reactions, and comments on Facebook”?

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 3.4.4

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.4.4

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.4

``` r
fb<-read.csv("file:///C:/Users/John/Documents/R/fact-checking-facebook-politics-pages/facebook-fact-check.csv")

str(fb)
```

    ## 'data.frame':    2282 obs. of  12 variables:
    ##  $ account_id    : num  1.84e+14 1.84e+14 1.84e+14 1.84e+14 1.84e+14 ...
    ##  $ post_id       : num  1.04e+15 1.04e+15 1.04e+15 1.04e+15 1.04e+15 ...
    ##  $ Category      : Factor w/ 3 levels "left","mainstream",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Page          : Factor w/ 9 levels "ABC News Politics",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Post.URL      : Factor w/ 2282 levels "https://www.facebook.com/ABCNewsPolitics/posts/1035057923259100",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Date.Published: Factor w/ 7 levels "2016-09-19","2016-09-20",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Post.Type     : Factor w/ 4 levels "link","photo",..: 4 1 1 1 4 1 4 1 1 4 ...
    ##  $ Rating        : Factor w/ 4 levels "mixture of true and false",..: 4 3 3 3 3 3 3 3 3 3 ...
    ##  $ Debate        : Factor w/ 2 levels "","yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ share_count   : int  NA 1 34 35 568 23 46 7 7 152 ...
    ##  $ reaction_count: int  146 33 63 170 3188 28 409 62 39 278 ...
    ##  $ comment_count : int  15 34 27 86 2815 21 105 64 6 59 ...

``` r
summary(fb)
```

    ##    account_id           post_id                Category   
    ##  Min.   :6.232e+10   Min.   :5.511e+14   left      : 471  
    ##  1st Qu.:1.145e+14   1st Qu.:1.247e+15   mainstream:1145  
    ##  Median :1.841e+14   Median :1.291e+15   right     : 666  
    ##  Mean   :1.867e+14   Mean   :3.300e+15                    
    ##  3rd Qu.:3.469e+14   3rd Qu.:1.541e+15                    
    ##  Max.   :4.401e+14   Max.   :1.015e+16                    
    ##                                                           
    ##                 Page    
    ##  Politico         :536  
    ##  CNN Politics     :409  
    ##  Eagle Rising     :286  
    ##  Right Wing News  :268  
    ##  Occupy Democrats :209  
    ##  ABC News Politics:200  
    ##  (Other)          :374  
    ##                                                             Post.URL   
    ##  https://www.facebook.com/ABCNewsPolitics/posts/1035057923259100:   1  
    ##  https://www.facebook.com/ABCNewsPolitics/posts/1035269309904628:   1  
    ##  https://www.facebook.com/ABCNewsPolitics/posts/1035305953234297:   1  
    ##  https://www.facebook.com/ABCNewsPolitics/posts/1035322636565962:   1  
    ##  https://www.facebook.com/ABCNewsPolitics/posts/1035352946562931:   1  
    ##  https://www.facebook.com/ABCNewsPolitics/posts/1035366579894901:   1  
    ##  (Other)                                                        :2276  
    ##     Date.Published Post.Type                          Rating    
    ##  2016-09-19:306    link :1780   mixture of true and false: 245  
    ##  2016-09-20:317    photo: 207   mostly false             : 104  
    ##  2016-09-21:306    text :   4   mostly true              :1669  
    ##  2016-09-22:293    video: 291   no factual content       : 264  
    ##  2016-09-23:294                                                 
    ##  2016-09-26:403                                                 
    ##  2016-09-27:363                                                 
    ##  Debate      share_count      reaction_count     comment_count     
    ##     :1984   Min.   :      1   Min.   :     2.0   Min.   :     0.0  
    ##  yes: 298   1st Qu.:     24   1st Qu.:   149.0   1st Qu.:    37.0  
    ##             Median :     96   Median :   545.5   Median :   131.5  
    ##             Mean   :   4045   Mean   :  5364.3   Mean   :   516.1  
    ##             3rd Qu.:    739   3rd Qu.:  2416.8   3rd Qu.:   390.2  
    ##             Max.   :1088995   Max.   :456458.0   Max.   :159047.0  
    ##             NA's   :70        NA's   :2          NA's   :2

``` r
#check which types of stories recieved more engagement
table(fb$Post.Type)
```

    ## 
    ##  link photo  text video 
    ##  1780   207     4   291

``` r
table(fb$Rating)
```

    ## 
    ## mixture of true and false              mostly false 
    ##                       245                       104 
    ##               mostly true        no factual content 
    ##                      1669                       264

``` r
View(fb)
```

Including Plots
---------------

You can also embed plots, for example:

![](facebook_factcheck_files/figure-markdown_github/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
