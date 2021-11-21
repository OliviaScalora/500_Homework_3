# 500_Homework_3

According to the US Department of Transportation, almost 30 people a day – or approximately one person every 51 minutes – die in motor vehicle crashes that involve an alcohol-impaired driver. Many more individuals are injured in these crashes. A recent study conducted by the National Highway Traffic Safety Administration has shown that the economic impact of alcohol-related crashes is estimated to be more than $59 billion annually. For more information, see the website of the Centers for Disease Control and Prevention.
The goal of the current assignment is to identify predictors of accidents related to drunk driving. The data used in this assignment come from a data set containing all 53,260 car crashes in the City of Philadelphia for the years 2008 – 2012. The data set was compiled by the Pennsylvania Department of Transportation, and is made available to the public at OpenDataPhilly.org. In the past, Azavea, one of Philadelphia’s most prominent GIS software development firms, has used these data for a number of interesting analyses, which have been published on the company’s website.
Because the crash data are geocoded, it is possible to spatially join the data to the 2000 Census block group level data set that was used for the two previous homework assignments. After the spatial join, each crash point contains the median household income and the percent of individuals with at least a bachelor’s degree in the block group where the crash took place.
The data set that you are given for this assignment, Logistic Regression Data.csv, contains the following variables:

1) CRN: Crash Record Number
2) DRINKING_D: Drinking driver indicator (1 = Yes, 0 = No)
3) COLLISION: Collision category that defines the crash (0 = Non collision, 1 = Rear-end, 2 =
Head-on, 3 = Rear-to-rear (Backing), 4 = Angle, 5 = Sideswipe (same dir.), 6 = Sideswipe (Opposite dir.), 7 = Hit fixed object, 8 = Hit pedestrian, 9 = Other or Unknown)
4) FATAL_OR_M: Crash resulted in fatality or major injury (1 = Yes, 0 = No)
5) OVERTURNED: Crash involved an overturned vehicle (1 = Yes, 0 = No)
6) CELL_PHONE: Driver was using cell phone (1= Yes, 0 = No)
7) SPEEDING: Crash involved speeding car (1 = Yes, 0 = No)
8) AGGRESSIVE: Crash involved aggressive driving (1 = Yes, 0 = No)
9) DRIVER1617: Crash involved at least one driver who was 16 or 17 years old (1 = Yes, 0 =No)
10) DRIVER65PLUS: Crash involved at least one driver who was at least 65 years old (1 = Yes, 0 = No)
11) AREAKEY: ID of the Census Block Group where the crash took place
12) PCTBACHMOR: % of individuals 25 years of age or older who have at least a bachelor’s
degree in the Census Block Group where the crash took place
13) MEDHHINC: Median household income in the Census Block Group where the crash took place

Even though the original data set has a total of 53,260 car crashes, for the sake of this
assignment, we remove the 9,896 crash locations which took place in non-residential block
groups, where median household income and vacancy rates are 0, from the data set. The final
data set contains the 43,364 crashes that took place in Philadelphia’s residential block groups.
Here, we will be regressing the binary dependent variable, DRINKING_D, on the following
binary and continuous predictors: FATAL_OR_M, OVERTURNED, CELL_PHONE, SPEEDING,
AGGRESSIVE, DRIVER1617, DRIVER65PLUS, PCTBACHMOR, and MEDHHINC.