#' ---
#' title: "Bioinformatics Class 5"
#' author: "Natalie Toothacre"
#' output:
#'   html_document:
#'     code_folding: hide
#' ---


#' Class 5 Graphs


#Boxplot
boxplot(rnorm(1000,0))

#Histogram
hist(rnorm(1000,0))

#Summary Statistics
summary(rnorm(1000,0))

#Flip my boxplot
boxplot(rnorm(1000,0), horizontal = TRUE)

#Read first data file
baby <- read.table("bimm143_05_rstats/weight_chart.txt", header = TRUE)

#Point and Line Plot
palette(c(rainbow(10)))
plot(baby, type = "b", pch = 8, cex = 2, lwd = 2, ylim = c(2,10), 
     xlab = "Age (months)", ylab = "Weight (kg)", 
     main = "Baby Weight over Time", col = 1:10)

#Read second data file
feat <- read.table("bimm143_05_rstats/feature_counts.txt", sep = "\t", header = TRUE)

#Bar Plot
par(mar=c(5, 14,4,2))
barplot(feat[,2], horiz = TRUE, names.arg = feat[,1], 
        main = "Features in Mouse Genome", las = 1)
title(ylab = "Number of Features", line = 10)

#Section 2
file <- "bimm143_05_rstats/male_female_counts.txt"
mf_counts <- read.table(file, sep = "\t", header = TRUE)

#Section 2 Bar Plot
barplot(mf_counts[,2], names.arg = mf_counts[,1], 
        horiz = TRUE, las = 1, col = c("blue2", "red2"))
title(ylab = "Gender", line = 8)

#Gene Expression
file2 <- "bimm143_05_rstats/up_down_expression.txt"
expression <- read.delim(file2)

#Scatterplot1
palette(c("red", "green", "blue"))
plot(expression$Condition1, expression$Condition2, col = expression$State)





#Section 2C

map.colors <- function (value,high.low,palette) {
  proportion <- ((value-high.low[1])/(high.low[2]-high.low[1]))
  index <- round ((length(palette)-1)*proportion)+1
  return (palette[index])
}

map.colors2 <- function (x, 
                         low.high = range(x), 
                         palette = cm.colors(100)) {
  
  ## Description: Map the values of the input vector 'x'
  ## to the input colors vector ‘palette' 
  
  #Determine percent values of the 'high.low' range
  percent <- ((x - low.high[1])/(low.high[2] - low.high[1]))
  
  #Find corresponding index position in the color 'palette'
  #  note catch for 0 percent values to 1
  index <- round ((length(palette) - 1) * percent) + 1
  
  
  return (palette[index])
}










