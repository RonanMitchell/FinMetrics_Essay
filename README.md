The purpose of this README is to guide the reader through the decisions
I made when writing my final Financial Econometrics research assignment
(Masters in Economics). I selected the following question:

-   “Study bond yield spreads - you have 10yr and 2yr bond yields.
    Calculate the spread and see if you can find any interesting
    patterns comparing yield spreads through time, linking it to VIX /
    currency volatility - and perhaps looking at other macro factors
    too.”

This README will guide the reader through any code, functions, and
output that was constructed for this assignment.

# Interpretation of the Question

I have decided to focus on a comparison between South African bond
yields and those of the rest of the world. I begin with descriptive data
analysis, such as simply plotting the various yields - except I
contextualize these yields by indicating high volatility times for the
US and above average VIX periods. I also compare the distribution of
yields for each country through the use of ridge lines. Finally, a
visually intuitive tool - geospatial analysis - is used to convey they
risk level of different countries.

# Required Packages:

## General Packages

-   tidyverse

## Plotting Packages

-   ggplot2

-   gridExtra

-   ggridges

-   viridis

-   hrbrthemes

-   lwgeom

-   sf

-   rnaturalearth

-   rnaturalearthdata

# Loading and Editing Data sets

## Importing Perscribed Data

The data sets all required similar wrangling, so I wrote a function to
sort of get each data set into similar shape (Data_Creator_foo). The
function essentially removes unnecessary tags from country names or bond
tickers (so that I can more easily merge all sets later on), and
converts to monthly yields. The reason for this is that I could only get
monthly inflation from the World Bank and in case I want to do some
yield adjustments later on, this change might be useful. Finally, the
function filters only by dates post-2010, because the SA data sets were
limited to this.

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
data sets might include the same names and bonds now, but do not have
the same dimensions. This need not usually be a huge concern, because
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

rm(list = c("Global_10yr", "Global_2yr", "SA_10yr", "SA_2yr"))
```

## Search Vectors

Below, I simply create some search/character vectors that will easily
allow me to filter through the data and only plot specific regions later
on. This will help for the ridge line plots, and the bond yield plots -
even the inflation plots to come. Region specific analysis makes more
sense to me because one can otherwise get lost in a mess of lines or
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
                          
                          !Bonds2yr$Name %in% Dev_Count]
        
)

# Venezuela destroyed the aesthetics of all my graphs because their yield, inflation and distribution are all ridiculous. It was visually easier to cope if they are not in the ridge or line plots. 
```

## Non-perscribed Data

Below I download CPI data from the World Bank. This is so that I am able
to either use CPI or inflation measures from a reputable source to
contextualized bond yields. Inflation is one of the only true economic
risks that can face a bond - in normal times - and as such I think it is
worth including in the analysis. I have also downloaded VIX and US-EU
exchange rate data from FRED.

With this VIX and US exchange rate data, I will not really be plotting
them in a traditional way. They will be used to extract high volatility
times so that I can add them to the line graphs as geom_rect() objects
so as to be a contextual backdrop for the inflation and yield movements
we observe. This will aid the discussion.

``` r
# world bank CPI measure, I have only downloaded since 2010. 

CPI <- read.csv("data/Inflation Data.csv") %>%
    
  `colnames<-`(sub("^X", "", colnames(.))) %>% # Excel problem
    
  mutate(Name = ifelse(Name == "China", "CHINA",
                       
                ifelse(Name == "New Zealand", "NZ",
                              
                ifelse(Name == "United Kingdom", "UK", 
                                     
                ifelse(Name == "United States", "US",
                       
                ifelse(Name == "Russian Federation", "Russia",
                       
                ifelse(Name == "Venezuela, RB", "Venezuela",
                                            
                ifelse(Name == "South Africa", "ZA", Name)
                
                ))))))) %>%
    
  filter(Name %in% Bonds2yr$Name) %>% # select same countries
    
  gather(date, CPI, -Name) %>%
    
  mutate(date = as.Date(paste0(date, "-01-01"))) %>% 
    
  arrange(Name, date) %>%
    
  group_by(Name) %>%
    
  mutate(Inflation = (CPI - lag(CPI)) / lag(CPI) * 100) %>% # inflation
    
  replace_na(list(Inflation = 0)) %>% # first date is zero inflation 
    
  mutate(CPI = (CPI - 100)) %>% 
    
  ungroup()
```

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

VIX_Vol <- High_Vol_foo(VIX_Data, VIX, 0.67, type = "")

US_Vol <- High_Vol_foo(US_Exchange, Rate, 0.67, type = "sd")

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
yield plot, I also plot the inflation of each country.

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



Yield_Plotter(Data = Bonds10yr,
              Vec = Dev_Count, 
              y = BondYield_10,
              "Developed Nations",
              "Bond Yield (10-Year)",
              "none")
```

<img src="README_files/figure-markdown_github/unnamed-chunk-8-1.png" width="100%" height="100%" />

``` r
Yield_Plotter(Data = CPI,
              Vec = Dev_Count, 
              y = Inflation,
              "",
              "Inflation",
              "bottom")
```

<img src="README_files/figure-markdown_github/unnamed-chunk-8-2.png" width="100%" height="100%" />

## Developing Nations Bond Yields & Inflation

``` r
Yield_Plotter(Data = Bonds10yr,
              Vec = Developing_vec, 
              y = BondYield_10,
              "Developing Nations",
              "Bond Yield (10-Year)",
              "none")
```

<img src="README_files/figure-markdown_github/unnamed-chunk-9-1.png" width="100%" height="100%" />

``` r
Yield_Plotter(Data = CPI,
              Vec = Developing_vec, 
              y = Inflation,
              "",
              "Inflation",
              "bottom")
```

<img src="README_files/figure-markdown_github/unnamed-chunk-9-2.png" width="100%" height="100%" />

## BRICS Bond Yields & Inflation

``` r
Yield_Plotter(Data = Bonds10yr,
              Vec = BRICS, 
              y = BondYield_10,
              "BRICS",
              "Bond Yield (10-Year)",
              "none")
```

<img src="README_files/figure-markdown_github/unnamed-chunk-10-1.png" width="100%" height="100%" />

``` r
Yield_Plotter(Data = CPI,
              Vec = BRICS, 
              y = Inflation,
              "",
              "Inflation",
              "bottom")
```

<img src="README_files/figure-markdown_github/unnamed-chunk-10-2.png" width="100%" height="100%" />

``` r
# Housekeeping 

rm(
    
    list = c("HV1", "HV2", "HV3", "HV4", "HV5", "HV6", "HV7", "HV8")
    
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

<img src="README_files/figure-markdown_github/unnamed-chunk-12-1.png" width="100%" height="100%" />

``` r
Ridge_Plotter(Bonds10yr,
              "US", # American mean bond yield dotted line
              BondYield_10, 
              Dev_Count,
              "Distribution of Developed Countries Yields")
```

    ## Picking joint bandwidth of 0.286

<img src="README_files/figure-markdown_github/unnamed-chunk-13-1.png" width="100%" height="100%" />

## Geospatial Plotting

With geo-spatial plotting, the general issue that most would have is to
get the data into the correct format. It must be an sf object, but you
can download these from the geo-spatial packages in my package selection
from earlier. Inspect the Map_Data_foo carefully, it simply takes the
necessary steps in a repeatable process to merge the data sets in this
case to the format required.

``` r
source("code/Map_Data_foo.R")

M1 <- Map_Data_foo(Bonds2yr, BondYield_2)
M2 <- Map_Data_foo(CPI, Inflation)
```

Once the data is in the correct format, plotting simply requires ggplot.
This function can do that, and does so repeatably. The big step is that
it requires the geom_sf() notation in the plot, but other than that, it
is plotted as usual and can be customized in identical ways.

This geo-spatial plot simply demonstrates that being categorized by bond
yield as high risk is not to be equated with being high risk due to
inflation. There is definitely correlation, but the shade of countries
is not identical in the two scenarios.

``` r
source("code/Create_Map_foo.R")

###

Yield_Map <- create_map(M1, "Average Bond Yield (2-Year)", "none")

Inflation_Map <- create_map(M2, "Average Inflation", "none")

###

print(Yield_Map)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-15-1.png" width="100%" height="100%" />

``` r
print(Inflation_Map)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-15-2.png" width="100%" height="100%" />
