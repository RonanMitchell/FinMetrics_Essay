This is the README for an R project. The purpose of this README is to
guide the reader through the programming decisions I made when plotting
bond yields and calculating yield spreads in order to answer the
following question:

- *“Study bond yield spreads - you have 10yr and 2yr bond yields.
  Calculate the spread and see if you can find any interesting patterns
  comparing yield spreads through time, linking it to VIX / currency
  volatility - and perhaps looking at other macro factors too.”*

This README will guide the reader through any code, functions, and
output that was constructed for this assignment. This is not the final
research assignment, which will be found in a PDF document made using
Texevier (<https://github.com/Nicktz/Texevier>) and will be named
“Essay_PDF” inline with the separate Rmd file constructed for that
purpose. The order of the graphs in this README will not always match
the order of graphs in the final essay, as this README is a
representation of the order in which the ideas came to me, and how they
flowed. The essay is a structured academic piece.

# Interpretation of the Question

As I plotted the yields over time, yield distributions, and geo-spatial
analysis, I noticed two interesting trends which were later confirmed by
the yield spread analysis. Firstly, the United States government is no
longer the safest asset in developed world, and at times, it has
actually been the riskiest asset on the developed bonds market.
Secondly, South Africa has become perceived as incredibly risky since
2015 (likely related to becoming junk status), even compared to BRICS
counterparts like China and India. In fact, as the United States has
become riskier, South Africa has become even more risky relative to the
United States. I decided to focus on these trends.

# Required Packages:

## General Packages

- tidyverse

## Plotting Packages

- ggplot2

- gridExtra

- ggridges

- viridis

- hrbrthemes

- lwgeom

- sf

- rnaturalearth

- rnaturalearthdata

# Loading and Editing Data sets

## Importing Perscribed Data

The provided data sets all required similar wrangling, so I wrote a
function to get each set into similar shape (Data_Creator_foo). The
function essentially removes unnecessary tags from country names or bond
tickers (so that I can easily merge all sets later on), and selects end
of month yields. The function filters only by dates post-2010, because
the SA data sets were limited to this time period, and as the question
relates to South Africa (in my interpretation), this change made sense.

``` r
source("code/Data_Creator_foo.R")

# 2 year Global Bonds.

Global_2yr <- 

    Data_Creator_foo(readRDS("data/BondYields_2Y.rds"),
                 
                     "_2yr",
                 
                     "2Y")

  # 10 year Global bonds.

Global_10yr <- 
    
    Data_Creator_foo(readRDS("data/BondYields_10Y.rds"),
                     
                     "_10Yr",
                     
                     "10Y")  %>%
   
   filter(Name %in% Global_2yr$Name)



  # 2 year South African (SA) bonds. 

SA_2yr <- 
    
    Data_Creator_foo(readRDS("data/BondYields_2Y_SA.rds"),
                     
                     "_2Yr", 
                     
                     "")

  # 10 Year SA bonds.

SA_10yr <- 
    
    Data_Creator_foo(readRDS("data/BondYields_10Y_SA.rds"),
                     
                     "_10Yr",
                     
                     "") 
```

The code chunk below simply demonstrates my investigation into why the
data sets might include the same names and bonds, but do not have the
same dimensions. This need not usually be a huge concern, because
plotting will work regardless, but I just wanted to check whether there
is cause for concern. In the end, it was fine and nothing much really
needed changing. In order to check the similarity of different columns
in each data set, I wrote a comparison function named
Similarity_Checker_foo.R.

``` r
# Why are the data sets different lengths? 

source("code/Similarity_Checker_foo.R")

SimilarityChecker_foo(Global_10yr, Global_2yr, "date") # same dates 
```

    ## [1] "The columns are identical."

``` r
SimilarityChecker_foo(Global_10yr, Global_2yr, "Name") # same countries 
```

    ## [1] "The columns are identical."

``` r
SimilarityChecker_foo(
    
    Global_10yr, Global_2yr, "Ticker"
    
    ) # different bonds but same countries? 
```

    ## [1] "The columns differ."

``` r
###

Global_2yr <- 
    
    Global_2yr %>% 
    
    filter(
        
        Ticker %in% Global_10yr$Ticker) %>% 

# Removed some but still different number of observations. 
    
    #Duplicate dates? 

 distinct(Ticker, date, .keep_all = TRUE) # no duplicate dates 

# print(table(Global_10yr$Name))

# print(table(Global_2yr$Name))

# Different countries have different starting points in terms of dates, different numbers of observations each, between the two data sets. 
```

## Merging Perscribed Data

Because the data came in pseudo-long format, I had to write a function
to easily merge future data sets, as, from my point of view, merging
these data sets required some substantial but repeatable coding,
wrangling and thinking. So the function now does all of this for me -
Data_Merger_foo.R.

``` r
source("code/Data_Merger_foo.R")

###

Bonds2yr <- 

Data_Merger_foo(Global_2yr, SA_2yr, BondYield_2, "BondYield_2")

###

Bonds10yr <- 

Data_Merger_foo(Global_10yr, SA_10yr, BondYield_10, "BondYield_10")

###

# We no longer need these: 

rm(list = c("Global_10yr", "Global_2yr", "SA_10yr", "SA_2yr"))
```

## Search Vectors

Below, I simply create some search/character vectors that easily allow
me to filter through the data and only plot specific regions later on.
This will help for the ridge line plots, and the bond yield plots - even
the inflation plots to come. Region specific analysis makes more sense
to me because one can otherwise get lost in a mess of lines or
distributions that exhibit less than usual (visual) co-movement, and it
can be difficult to see exactly what is going on.

``` r
# Isolating BRCIS countries: 

BRICS <- c(
    
    "Brazil", 
          
    "CHINA", 
          
    "India", 
           
    "Russia",
    
    "ZA"
    
    )

# Developed Countries: 

Dev_Count <- c(
    
    "Canada",
    
    "France",
    
    "Germany",
    
    "Japan",
    
    "Korea",
    
    "NZ",
    
    "Singapore",
    
    "UK",
    
    "US"
    
    )

# Developing Countries: 

Developing_vec <- 
    
    unique(
  
        Bonds2yr$Name[!grepl("EURO", Bonds2yr$Name) & 
                          
                        !grepl("Venezuela", Bonds2yr$Name) &
                          
                          !grepl("Mexico", Bonds2yr$Name) &
                          
                            !Bonds2yr$Name %in% Dev_Count])

# Venezuela and Mexico destroyed the aesthetics of all my graphs because their yield, inflation and distribution are all ridiculous. It was visually easier to cope if they are not in the ridge or line plots. I will still discuss them in the essay.
```

## Non-perscribed Data

Below I download CPI, GDP Growth and debt to GDP data from the World
Bank. This is so that I am able to either use debt ratios, inflation, or
GDP measures from a reputable source to contextualize bond yields.
Inflation is one of the only pure economic risks that can face a bond -
in good times - and as such I think it is worth including in the
analysis. I created a function to import the data from the World Bank as
it is always in the same, messy and unclean format. So World_Bank_foo()
gets it into the format I want. This was useful as I downloaded many
data sets.

I have also downloaded VIX and US-EU exchange rate data from FRED. With
this VIX and US exchange rate data, I will not really be plotting them
in a traditional way. They will be used to extract high volatility times
so that I can add them to the line graphs as geom_rect() objects so as
to be a contextual backdrop for the inflation and yield movements we
observe. This will aid the discussion.

``` r
# Search vector

All_Names <- unique(Bonds2yr$Name)

###

source("code/World_Bank_foo.R")

# World Bank growth rates: 

Growth_Rates <- 
    
    World_Bank_foo(read.csv("data/GDP Growth.csv"), # import data
                   
                   All_Names, # only these countries 
                   
                   Growth) # what to name gathered column

# IMF Debt to GDP: 

Debt_GDP <- 
    
    World_Bank_foo(read.csv("data/DebtToGDP.csv"), All_Names, Debt_to_GDP) %>% 
    
    mutate(Debt_to_GDP = as.numeric(
        
        ifelse(
        
        Debt_to_GDP == "no data", NA, Debt_to_GDP)))

# world bank CPI measure, I have only downloaded since 2010. 

CPI <- 
    
  World_Bank_foo(read.csv("data/Inflation Data.csv"), All_Names, CPI) %>% 
    
    # I also create an inflation column: 
    
  arrange(Name, date) %>%
    
  group_by(Name) %>%
    
  mutate(Inflation = (CPI - lag(CPI)) / lag(CPI) * 100) %>% # inflation
    
  filter(date > "2009-07-01") %>% # first date is zero inflation 
    
  mutate(CPI = (CPI - 100)) %>% 
    
  ungroup()

###
```

In the following chunk you will notice High_Vol_foo(). This function
calculates high volatility periods in a variable, either by standard
deviation (exchange rate) or mean (for the VIX, as a high VIX is already
indicative of volatility), and selects whatever quantile() is specified.
You can specify the data set, variable, quantile, and method (either
above mean or standard deviation). The last part is achieved with an
if/else function within the function.

``` r
###

# VIX monthly data from FRED:

VIX_Data <- read.csv("data/VIX Data.csv") %>%
  
    mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
    
        filter(date <= "2022-03-31")

###

US_Exchange <- read.csv("data/US to EU Exchange.csv") %>%
  
    mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
    
        filter(date <= "2022-03-31")

###

source("code/High_Vol_foo.R")

VIX_Vol <- High_Vol_foo(VIX_Data, VIX, 0.8, type = "")

US_Vol <- High_Vol_foo(US_Exchange, Rate, 0.8, type = "sd")

# These high volatility periods are used to construct the geom_rects to indicate them on the ggplot graphs.

###

# I tried for hours to think of a functional way to create these rectangles. i.e. I thought maybe a for loop of some kind, i.e. for date in VIX_Vol etc... but I found it incredibly difficult to add whatever result I came up with neatly to a ggplot as an object. There probably is a very easy way to do it but it stumped me for quite some time. 

source("code/Rectangle_Creation_Script.R")

alpha = 0.15
```

# Bond Yields & Inflation

Below, I plot the 10-year bond yields of three distinct regions. First,
I plot the developed world, which exhibits considerable similarity in
behavior. I also plot the developing world, and BRICS. Below each Bond
yield plot, I also plot the yearly inflation of each country.

In these plots, light red zones are high VIX level time periods. The
blue zones are high US exchange rate volatility time periods. If a zone
is purple, it means these two zones overlap. In a zone such as this, one
would expect considerably unique behavior - either from yields or from
inflation.

The function created for this is named Yield_Plotter_foo. This is a poor
name, in fact, because it can actually plot pretty much any variable
that is in a tidy data set and requires a line graph to be drawn. It is
also used to plot the inflation in each case, for example.

## Developed Nations Bond Yields & Inflation

``` r
source("code/Yield_Plotter_foo.R")

###

grid.arrange(

Yield_Plotter(Data = Bonds2yr,
              Vec = Dev_Count, 
              y = BondYield_2,
              "Developed Nations",
              "Bond Yield (2-Year)",
              "none"),

Yield_Plotter(Data = CPI,
              Vec = Dev_Count, 
              y = Inflation,
              "",
              "Inflation",
              "bottom"),

ncol = 1,

heights = c(1.5,2.1))
```

<img src="README_files/figure-gfm/unnamed-chunk-8-1.png" width="100%" height="100%" />

## Developing Nations Bond Yields & Inflation

``` r
grid.arrange(

Yield_Plotter(Data = Bonds2yr,
              Vec = Developing_vec, 
              y = BondYield_2,
              "Developing Nations",
              "Bond Yield (2-Year)",
              "none"),

Yield_Plotter(Data = CPI,
              Vec = Developing_vec, 
              y = Inflation,
              "",
              "Inflation",
              "bottom"),

ncol = 1,

heights = c(1.5,2.1))
```

<img src="README_files/figure-gfm/unnamed-chunk-9-1.png" width="100%" height="100%" />

## BRICS Bond Yields & Inflation

``` r
grid.arrange(

Yield_Plotter(Data = Bonds2yr,
              Vec = BRICS, 
              y = BondYield_2,
              "BRICS",
              "Bond Yield (2-Year)",
              "none"),

Yield_Plotter(Data = CPI,
              Vec = BRICS, 
              y = Inflation,
              "",
              "Inflation",
              "bottom"),

ncol = 1,

heights = c(2,2.5))
```

<img src="README_files/figure-gfm/unnamed-chunk-10-1.png" width="100%" height="100%" />

``` r
# You no longer need any of these, keep your space clean:

rm(
    
    list = c("HV1", "HV2", "HV5", "HV6", "HV7", "HV8")
    
    )

rm(
    
    list = c("VIX_Data", "US_Exchange", "US_Vol", "VIX_Vol")
    
    )

rm(list = "alpha")
```

## Ridge Plots

Here, I simply plot the distribution of bond yields for all countries in
the developed world, and BRICS - in a ridge line setup. The function
created for this is Ridge_Plotter_foo. The dotted line represents, in
the case of the developed world, the mean bond yield of the USA. In the
case of the BRICS nations, it represents the mean bond yield of South
Africa.

``` r
source("code/Ridge_Plotter_foo.R")

###

Ridge_Plotter(Bonds10yr, 
              "ZA", # South African mean bond yield dotted line
              BondYield_10,
              BRICS,
              "Distribution of BRICS Yields")
```

    ## Picking joint bandwidth of 0.236

<img src="README_files/figure-gfm/unnamed-chunk-12-1.png" width="100%" height="100%" />

``` r
Ridge_Plotter(Bonds10yr,
              "US", # American mean bond yield dotted line
              BondYield_10, 
              Dev_Count,
              "Distribution of Developed Countries Yields")
```

    ## Picking joint bandwidth of 0.286

<img src="README_files/figure-gfm/unnamed-chunk-13-1.png" width="100%" height="100%" />

## Geospatial Plotting

With geo-spatial plotting, the general issue that most would have is to
get the data into the correct format. It must be an *sf* object, but you
can download these from the geo-spatial packages in my package selection
from earlier. Inspect the Map_Data_foo() carefully, it simply takes the
necessary steps in a repeatable process to merge the data sets in this
case to the format required. The biggest problem is usually that
different data sets use different naming conventions for country titles.
This is not an automatable process, and will always require manual
fixing, depending on your data set. At least the *sf* data sets
themselves have very intuitive naming conventions.

``` r
source("code/Map_Data_foo.R")

M1 <- Map_Data_foo(Bonds2yr, BondYield_2)
M2 <- Map_Data_foo(CPI, Inflation)
M3 <- Map_Data_foo(Growth_Rates, Growth)
M4 <- Map_Data_foo(Debt_GDP, Debt_to_GDP)
```

Once the data is in the correct format, plotting simply requires ggplot.
Create_Map_foo() can do this, and does so repeatably. The big step is
that it requires the geom_sf() notation in the plot, but other than
that, it is plotted as usual and can be customized in identical ways.

This geo-spatial plot simply demonstrates that being categorized by bond
yield as high risk is not to be equated with being high risk due to
inflation, or GDP growth. There is definitely correlation, but the shade
of countries is not directly correlated (positively or negatively) in
all scenarios.

``` r
source("code/Create_Map_foo.R")

###

create_map(M1, "Average Bond Yield (2-Year)", "none")
```

<img src="README_files/figure-gfm/unnamed-chunk-15-1.png" width="100%" height="100%" />

``` r
create_map(M2, "Average Inflation", "none")
```

<img src="README_files/figure-gfm/unnamed-chunk-16-1.png" width="100%" height="100%" />

``` r
create_map(M3, "Average GDP Growth", "none")
```

<img src="README_files/figure-gfm/unnamed-chunk-17-1.png" width="100%" height="100%" />

``` r
create_map(M4, "Average Debt-to-GDP Ratio", "none")
```

<img src="README_files/figure-gfm/unnamed-chunk-18-1.png" width="100%" height="100%" />

``` r
rm(list = c("M1", "M2", "M3", "M4"))
```

# Yield Spreads

## All Countries

In order to plot the yield spreads (to answer the main question), I
created Spread_Plotter(). It is a function that takes a data set,
subtracts the US yield from the yield of all other countries on all
dates, and then plots the resulting spread with a horizontal line at
zero to indicate the US-zero level. It also plots a pseudo-confidence
interval of the average yield spread of the selected countries. It is
just meant to shade the background to show you where the general spread
has narrowed and when it has widened.

``` r
source("code/Spread_Plotter.R")

###

grid.arrange(

   Spread_Plotter(Bonds2yr, 
                  BondYield_2, 
                  Dev_Count, 
                  "BondYield_2", 
                  "Developed Countries", 
                  "2-Year", 
                  "none"), 

   Spread_Plotter(Bonds10yr, 
                  BondYield_10, 
                  Dev_Count, 
                  "BondYield_10", 
                  "", 
                  "10-Year", 
                  "bottom"),

   ncol = 1,

   heights = c(1.5,2))
```

<img src="README_files/figure-gfm/unnamed-chunk-20-1.png" width="100%" height="100%" />

``` r
grid.arrange(

   Spread_Plotter(Bonds2yr, 
                  BondYield_2, 
                  BRICS, 
                  "BondYield_2",
                  "BRICS", 
                  "2-Year", 
                  "none"), 

   Spread_Plotter(Bonds10yr, 
                  BondYield_10, 
                  BRICS, 
                  "BondYield_10", 
                  "", 
                  "10-Year", 
                  "bottom"),

   ncol = 1,

   heights = c(1.5,2))
```

<img src="README_files/figure-gfm/unnamed-chunk-21-1.png" width="100%" height="100%" />

## Unique Pair Comparisons

For this section I once again created a spread plotter that first
calculates spreads and then directly plots these, but crucially, it can
only take two input countries as it creates a geom_ribbon between the
two. This is a wonderful plotting style for direct comparison between
two countries that might be in different groups or settings. I do not
claim to have come up with this style, I simply replicated it from other
Google searches of yield spreads.

``` r
source("code/Spread_Comparison_foo.R")

###

Spread_Comparison_foo(Bonds10yr, 
                      BondYield_10, 
                      "BondYield_10", 
                      c("ZA", "Germany"), 
                      c("Germany" = "lightblue", "ZA" = "pink"),
                      "10-Year Yield Spread Between South Africa and Germany")
```

<img src="README_files/figure-gfm/unnamed-chunk-22-1.png" width="100%" height="100%" />

``` r
Spread_Comparison_foo(Bonds10yr, 
                      BondYield_10, 
                      "BondYield_10", 
                      c("ZA", "CHINA"), 
                      c("CHINA" = "pink", "ZA" = "pink3"),
                      "10-Year Yield Spread Between South Africa and China")
```

<img src="README_files/figure-gfm/unnamed-chunk-23-1.png" width="100%" height="100%" />

``` r
Spread_Comparison_foo(Bonds10yr, 
                      BondYield_10, 
                      "BondYield_10", 
                      c("ZA", "India"), 
                      c("ZA" = "pink3", "India" = "pink"),
                      "10-Year Yield Spread Between South Africa and India")
```

<img src="README_files/figure-gfm/unnamed-chunk-24-1.png" width="100%" height="100%" />

# Bibliography

CBOE Volatility Index: VIX \[Online\]. \[n.d.\]. Available:
<https://fred.stlouisfed.org/series/VIXCLS> \[2023, December 10\].

Central Government Debt \[Online\]. \[n.d.\]. Available:
<https://www.imf.org/external/datamapper/CG_DEBT_GDP@GDD/CHN/FRA/DEU/ITA/JPN/GBR/USA>
\[2023, December 28\].

Consumer Price Index (2010 = 100) \[Online\]. \[n.d.\]. Available:
<https://data.worldbank.org/indicator/FP.CPI.TOTL?end=2022&start=2010&view=chart>
\[2023, December 10\].

U.S. to Euro Spot Exchange Rate \[Online\]. \[n.d.\]. Available:
<https://fred.stlouisfed.org/series/DEXUSEU> \[2023, December 10\].

World Bank Group country classifications by income level for FY24
\[Online\]. \[n.d.\]. Available:
<https://blogs.worldbank.org/opendata/new-world-bank-group-country-classifications-income-level-fy24>
\[2023, December 31\].
