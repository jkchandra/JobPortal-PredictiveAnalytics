#########################################################################################################
# BC2407 Semester Analytics Project: Model Predicting Similar Skills
# Team Number 8
# Members: Christopher Gerard Toh, Teo Tian Shun Kenneth, Lim De Quan, Jonathan Kevin Chandra
# Datasets: skills_title_aggregate.csv
# Library: arules, arulesViz
####################################################################################################

setwd("C:/Users/jkchandra/Desktop/BC2407/Semester Project")
Job_skills <- read.transactions(file ="skills_title_aggregate.csv",sep=",")




#Arules=======================================================================================================


#All Skills======================================================================================
library(arules)
library(arulesViz)
rules1 <- apriori(Job_skills, parameter = list(supp = 0.01, conf = 0.05, 
                                               target = "rules",minlen=2, maxlen=2))
#remove dups and redundant
rules1<- rules1[!is.redundant(rules1)]
gi <- generatingItemsets(rules1)
d <- which(duplicated(gi))

inspect(head (sort(rules1[-d],decreasing = T, by = "lift"),n = 1000))
plot(rules1[-d], method ="graph",control=list(arrowSize=0.1))



#Specific Skills=====================================================================================
rules2 <- apriori(Job_skills, parameter = list(supp = 0.01, conf = 0.05, target = "rules",minlen=2),
                  appearance = list(lhs=c("html"),default="rhs"))
#remove dups and redundant
rules2<- rules2[!is.redundant(rules2)]
inspect(head (sort(rules2,decreasing = T, by = "lift"),n = 1000))

plot(rules2, method ="graph",control=list(arrowSize=0.1))

#Export for Power BI----------------------------------------------------------------------------------------------------
df = data.frame(
  lhs = labels(lhs(rules1[-d])),
  rhs = labels(rhs(rules1[-d])), 
  rules1[-d]@quality)

write.csv(df,file="Arules.csv", row.names = FALSE)