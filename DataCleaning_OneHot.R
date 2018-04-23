#########################################################################################################
# BC2407 Semester Analytics Project: One Hot Encoding of Skills
# Team Number 8
# Members: Christopher Gerard Toh, Teo Tian Shun Kenneth, Lim De Quan, Jonathan Kevin Chandra
# Datasets: skills_td_cut_syn.csv
# Library: 
########################################################################################################


setwd("C:/Users/jkchandra/Desktop/BC2407/Semester Project")


skills_one <- read.csv(file ="skills_td_cut_syn.csv")
skills_one$const = 1

# Remove duplicates
skills_one <- unique(skills_one)

# Need to reshape the matrix
skills_one_prep <- stats::reshape(data = skills_one,
                           idvar = "line",
                           timevar = "word",
                           direction = "wide")

# Clean up the missing values to be FALSE
skills_one_prep[is.na(skills_one_prep)] <- 0
colnames(skills_one_prep) <- gsub(x=colnames(skills_one_prep),
                               pattern="const\\.", replacement="")
#Set the dataframe as a matrix to be exported
skills_one_matrix <- as.matrix(skills_one_prep)
write.csv(skills_one_prep,file ="skills_one_hot.csv", row.names = FALSE)



