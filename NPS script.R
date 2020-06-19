load("~/NPS.RData")
variable.names(M)
class(M)
NPS <- data.frame(M)
library(dplyr)
NPS <- arrange(NPS, desc(TC))
selectedPapers <- filter(NPS, grepl('REICHHELD', CR))
rm(list=setdiff(ls(), "selectedPapers"))
selectedPapers$Against <- 1:nrow(selectedPapers)
balancedPapers <- selectedPapers[, c(1, 6, 53, 37)]
# Critical reading of each paper's abstract follows
balancedPapers$AB
# Then, we classified each paper's opinion
# on the use of NPS. Papers that used NPS without 
# providing any explicit critic in the abstract were classified
# as 0, while papers that provide explicit criticism on NPS
# were classified as 1, like this
balancedPapers$Against <- c(1,0,0,1,1,1,1,0,0,1,
                            1,0,0,0,0,0,0,0,0,0,
                            0,1,0,0,0,0,0,1,0,1,
                            0,0,0,1,0,0,0,1,0,0,
                            0,0,1,0,0,0,0,0,0,0,
                            0,0,0,0,1,0,0,1,0,0,
                            0,1,0,0,1,0,0,1,0,0,
                            0,0,0,0,0,0,0,0,0,0,
                            0,0,0,0,1,0,0,0,0,0,
                            0)
library(ggplot2)
p <- ggplot(balancedPapers, aes(x=TC, fill=as.character(Against))) + geom_density(alpha=0.3) + theme_classic() +
xlab("Number of Citations") +
theme(text = element_text(size=25),
      axis.text.x = element_text(size = 20), 
      axis.text.y = element_text(size = 20))

        p + theme(legend.position=c(x=0.8, y=0.8)) +
scale_fill_discrete(name = "Technical Orientation", labels = c("Promoting NPS use", "Criticizing NPS use"))  

ggplot(balancedPapers, aes(x=TC, fill=as.character(Against))) + geom_violin()
        
library(bibliometrix)
results <- biblioAnalysis(selectedPapers, sep = ";")

S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

authors=gsub(","," ",names(results$Authors))

indices <- Hindex(selectedPapers, field = "author", elements=authors, sep = ";", years = 50)
veamos <- indices$H


NetMatrix <- biblioNetwork(selectedPapers, analysis = "co-citation", network = "references", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)

##### Keywords co-occurrence
NetMatrix <- biblioNetwork(selectedPapers, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

# Conceptual Structure
CS <- conceptualStructure(selectedPapers,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)



# Plot a historical co-citation network
histResults <- histNetwork(selectedPapers, min.citations = 1, sep = ";")
net <- histPlot(histResults, n=25, size = 10, labelsize=5)


variable.names(selectedPapers)

