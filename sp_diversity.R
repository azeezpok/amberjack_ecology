####################Diversity was estimated by constructing rarefaction and extrapolation 
##curves using the multinomial probability distribution model on the Hill numbers 
###q = 0 (species richness), q = 1 (exponential of Shannon entropy index), and 
##q = 2 (the inverse of the Simpson concentration) for the sample-based incident data

setwd("D:/species diversity")

library(iNEXT)
library(ggplot2)

data<-read.csv("Cumulative prey2.csv")
str(data)
head(data)
data1<-as.data.frame(data)

as.incfreq(incidence_raw)
data(incidence_raw)
lapply(incidence_raw_data, as.incfreq)
iNEXT(incidence_raw_data, q=0, datatype="incidence_raw")
out <- iNEXT(incidence_raw_data, q=c(0, 1, 2), datatype="incidence_raw", endpoint=200)
ggiNEXT(out, type=1, facet.var="Assemblage")

# Load your data into a data frame
incidence_data <- read.table("path/to/your/data.txt", header = TRUE, sep = "\t")

# Rename columns if necessary
colnames(incidence_data) <- c("OTU", "Sample", "Abundance")

# Reshape the data frame so that each row represents a unique OTU-SampleID combination
incidence_raw <- reshape(incidence_data, idvar = "OTU", timevar = "Sample", direction = "wide")

# Rename columns to match the required format
colnames(incidence_raw) <- c("OTU", paste0("SampleID", 1:ncol(incidence_raw[,-1])),
                             paste0("Abundance", 1:ncol(incidence_raw[,-1])))
