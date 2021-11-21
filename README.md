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
9) DRIVER1617: