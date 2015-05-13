# ONE variables: 
# CATEGORICAL: 
    barplot(x[order(y, decreasing = TRUE)])

# Customize the chart
par(oma = c(1, 1, 1, 1))  # Sets outside margins: b, l, t, r ,
# will affect every plot, # until they are set differently
par(mar = c(4, 5, 2, 1))  # Sets plot margins

barplot(feeds[order(feeds)] 
        ,horiz  = TRUE
        ,las    = 1 # las gives orientation of axis labels
        , col    = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4")
        ,border = NA  # No borders on bars
        ,main   = "Frequencies of Different Feeds\nin chickwts Dataset" # \n = line break
        , xlab   = "Number of Chicks"
)

# HOW TO CHOOSE FONTS? I'D LIKE THAT! 
    
#     PIE CHARTS: DON'T USE .   

    
    #     QUANTITATIVE
    
    h <- hist(lynx
              ,  # Save histogram as object
              breaks = 11  # "Suggests" 11 bins
              #           breaks = seq(0, 7000, by = 100),
              #           breaks = c(0, 100, 300, 500, 3000, 3500, 7000),
             , freq = FALSE
              ,              col = "thistle1" # Or use: col = colors() [626]
              ,               main = "Histogram of Annual Canadian Lynx Trappings\n1821-1934"
              ,              xlab = "Number of Lynx Trapped"
              )
    
    # IF freq = FALSE, this will draw normal distribution
    curveve(dnorm(x, mean = mean(lynx), sd = sd(lynx)), 
          col = "thistle4", 
          lwd = 2,
          add = TRUE)
    
    #  boxplot
    boxplot(USJudgeRatings$RTEN
            , horizontal = TRUE
            ,las = 1 # Make all labels horizontal
            , notch = TRUE  # Notches for CI for median
            , ylim = c(3, 10) # Specify range on Y axis
            , col = "slategray3"  # R's named colors (n = 657)
            #         col = colors() [602], # R's color numbers
            #         col = "#9FB6CD",      # Hex codes for RBG
            #         col = rgb(159, 182, 205, max = 255),  # RGB triplet with max specified
            , boxwex = 0.5  # Width of box as proportion of original
            ,whisklty = 1 # Whisker line type; 1 = solid line
            ,  staplelty = 0  # Staple (line at end) type; 0 = none
            ,outpch = 16  # Symbols for outliers; 16 = filled circle
            ,outcol = "slategray3"  # Color for outliers
            ,main = "Lawyers' Ratings of State Judges in the\nUS Superior Court (c. 1977)"
            ,   xlab = "Lawyers' Ratings"
    )
    
    # Multiple boxplots
    boxplot(USJudgeRatings,
            horizontal = TRUE,
            las = 1,  # Make all labels horizontal
            notch = TRUE,  # Notches for CI for median
            ylim = c(2, 10),  # Specify range on Y axis
            col = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),   # R's named colors (n = 657)
            boxwex = 0.7,  # Width of box as proportion of original
            whisklty = 1,  # Whisker line type; 1 = solid line
            staplelty = 0,  # Staple (line at end) type; 0 = none
            outpch = 16,  # Symbols for outliers; 16 = filled circle
            outcol = "bisque4",  # Color for outliers
            main = "Lawyers' Ratings of State Judges in the\nUS Superior Court (c. 1977)",
            xlab = "Lawyers' Ratings")
    
    # SEE HOW TO CHANGE BORDER COLOR 
    
#     OVERLAYING PLOTS
    
    # PLOTS
    # Plot 1: Histogram
    h <- hist(fertility,
              prob = TRUE,  # Flipside of "freq = FALSE"
              ylim = c(0, 0.04),
              xlim = c(30, 100),
              breaks = 11,
              col = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
              border = 0,
              main = "Fertility for 47 French-Speaking\nSwiss Provinces, c. 1888")
    
    # Plot 2: Normal curve (if prob = TRUE)
    curve(dnorm(x, mean = mean(fertility), sd = sd(fertility)), 
          col = "bisque4", 
          lwd = 3,
          add = TRUE)
    
    # Plot 3 & 4: Kernel density lines (if prob = TRUE)
    lines(density(fertility), col = "blue")
    lines(density(fertility, adjust = 3), col = "darkgreen")
    
    # Plot 5: Rug (That is, lineplot under histogram)
    rug(fertility, col = "red")
    
    
    # Frequency Tables
    
    groups.t1 <- table(groups)  # Creates frequency table 
    
    # PROPORTIONS AND PERCENTAGES
    prop.table(groups.t1)  # Give proportions of total
    round(prop.table(groups.t1), 2)  # Give proportions w/2 decimal places
    round(prop.table(groups.t2), 2) * 100  # Give percentages w/o decimal places
    
    # DESCRIPTIVE STATISTICS
    > summary(cars$speed)  # Summary for one variable
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    4.0    12.0    15.0    15.4    19.0    25.0 
   > summary(cars)  # Summary for entire table
    speed           dist       
    Min.   : 4.0   Min.   :  2.00  
    1st Qu.:12.0   1st Qu.: 26.00  
    Median :15.0   Median : 36.00  
    Mean   :15.4   Mean   : 42.98  
    3rd Qu.:19.0   3rd Qu.: 56.00  
    Max.   :25.0   Max.   :120.00  
    # Tukey's five-number summary: minimum, lower-hinge,
     # median, upper-hinge, maximum. No labels.
        > fivenum(cars$speed)
    [1]  4 12 15 19 25
    
    # Boxplot stats: hinges, n, CI, outliers
#     look up how hinges and outliers are calculated
        > boxplot.stats(cars$speed)
    $stats
    [1]  4 12 15 19 25
    
    $n
    [1] 50
    
    $conf
    [1] 13.43588 16.56412
    
    $out
    numeric(0)
    require("psych")
    describe(cars)
          vars  n  mean    sd median trimmed   mad min max range  skew kurtosis   se
    speed    1 50 15.40  5.29     15   15.47  5.93   4  25    21 -0.11    -0.67 0.75
    dist     2 50 42.98 25.77     36   40.88 23.72   2 120   118  0.76     0.12 3.64
    
    
#     Hypothesis Testing: 
        
#         1- a Prop.Test: a x-squared test assuming a 50-50 chance of any outcome 
#     (blind chance). the test performs both types of tests :
#     goodness of fit and independence
    # 98 wins out of 162 games (default settings)
    prop.test(98, 162)
    
    # One-tailed test with 90% CI
    prop.test(98, 162, alt = "greater", conf.level = .90)
    
    heads <- rbinom(1, size = 100, prob = .5)
    prop.test(heads, 100)          # continuity correction TRUE by default
    prop.test(heads, 100, correct = FALSE)
    
    ## Data from Fleiss (1981), p. 139.
    ## H0: The null hypothesis is that the four populations from which
    ##     the patients were drawn have the same true proportion of smokers.
    ## A:  The alternative is that this proportion is different in at
    ##     least one of the populations.
    
    smokers  <- c( 83, 90, 129, 70 )
    patients <- c( 86, 93, 136, 82 )
    prop.test(smokers, patients)
    
    4-sample test for equality of proportions without continuity correction
    
    data:  smokers out of patients
    X-squared = 12.6004, df = 3, p-value = 0.005585
    alternative hypothesis: two.sided
    sample estimates:
        prop 1    prop 2    prop 3    prop 4 
    0.9651163 0.9677419 0.9485294 0.8536585 
    
#     since the p-value is lower than the conventionally accepted significance level 
#     of 0.05, we reject the null hypothesis of no statisticaly significant difference 
#     in the proportion of smokers in the 4 populations
    
#     t-test:
    
    > t.test(mag)
    
    One Sample t-test
    
    data:  mag
    t = 362.7599, df = 999, p-value < 2.2e-16
    alternative hypothesis: true mean is not equal to 0
    95 percent confidence interval:
        4.595406 4.645394
    sample estimates:
        mean of x 
    4.6204 
    
#    THE CHI-SQ TESST 
#     the prop test only test one contingency compared to a population, or one contingency over
#     several populations, which is why it is only suitable for testing the success/failur
#     i.e. binomail distributions, compared to expected, or over different populations
#     however the chisq.test is suitable for comparing any number of contingencies either wtih
#     an expected population (goodness of fit), or over different scenarios (independence 
#     of such scenarios)
    
    
    > chi2 <- chisq.test(eyes, p = c(.41, .32, .15, .12))
    > chi2
    
    Chi-squared test for given probabilities
    
    data:  eyes
    X-squared = 6.4717, df = 3, p-value = 0.09079
    
    
    # Robust methods for describing center:
    mean(area)  # NOT robust
    median(area)
    mean(area, trim = .05)  # 5% from each end (10% total)
    mean(area, trim = .10)  # 10% from each end (20% total)
    mean(area, trim = .20)  # 20% from each end (40% total)
    mean(area, trim = .50)  # 50% from each end = median
    
    # Robust methods for describing variation:
    sd(area)  # NOT robust
    mad(area)  # Median absolute deviation
    IQR(area)  # Interquartile range (Can select many methods)
    fivenum(area)  # Tukey's hinges (similar to quartiles)
    
    