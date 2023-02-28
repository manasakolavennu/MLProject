library(viridis)
library(arules)
library(TSP)
library(data.table)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)
library(arulesViz)


## YOUR working dir goes here...
setwd('/Users/manasak/Downloads/')

Data <- read.transactions("ClusterTrainDataBasket.csv",
                           rm.duplicates = FALSE, 
                           format = "basket",  ##if you use "single" also use cols=c(1,2)
                           sep=",",  ## csv file
                           cols=NULL) ## The dataset has no row numbers
inspect(Data)

##### Use apriori to get the RULES
Frules = arules::apriori(Data, parameter = list(support=0.01, 
                                                 confidence=1, minlen=2))
inspect(Frules)
inspect(Frules[126:173])

## Sort rules by a measure such as conf, sup, or lift
SortedRules_conf <- sort(Frules, by="confidence", decreasing=TRUE)
inspect(SortedRules_conf[1:15])
(summary(SortedRules_conf))

SortedRules_supp <- sort(Frules, by="support", decreasing=TRUE)
inspect(SortedRules_supp[1:15])
(summary(SortedRules_supp))

SortedRules_lift <- sort(Frules, by="lift", decreasing=TRUE)
inspect(SortedRules_lift[1:15])
(summary(SortedRules_lift))

plot(SortedRules_conf, method="graph", engine="htmlwidget", limit = 15)
plot(SortedRules_supp, method="graph", engine="htmlwidget", limit = 15)
plot(SortedRules_lift, method="graph", engine="htmlwidget", limit = 15)



