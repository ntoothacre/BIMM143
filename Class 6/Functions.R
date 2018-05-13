#Class 6 Bioinformatics

#Funtions

#First Funtion
add <- function(x, y=1) {
  # Sum the input x and y
  x + y
}

#Second Function
rescale <- function(x) {
  rng <-range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

#Test on small example where you know the answer
rescale(1:10)

# How would you get your function to work here…
rescale( c(1,2,NA,3,10) )

# What should your function do here?
rescale( c(1,10,"string") )


#Next generation rescaling
rescale2 <- function(x, na.rm=TRUE, plot=FALSE) {
  if(na.rm) {
    rng <-range(x, na.rm=na.rm)
  } else {
    rng <-range(x)
  }
  print("Hello")
  answer <- (x - rng[1]) / (rng[2] - rng[1])
  print("is it me you are looking for?")
  if(plot) {
    plot(answer, typ="b", lwd=4)
  }
  print("I can see it in ...")
}

#Lets test rescale
rescale2( c(1,2,NA,3,10))

#Returning answer
rescale3 <- function(x, na.rm=TRUE, plot=FALSE) {
  if(na.rm) {
    rng <-range(x, na.rm=na.rm)
  } else {
    rng <-range(x)
  }
  print("Hello")
  answer <- (x - rng[1]) / (rng[2] - rng[1])
  print("is it me you are looking for?")
  if(plot) {
    plot(answer, typ="b", lwd=4)
  }
  print("I can see it in ...")
  
  return(answer)
  
}

#Lets test rescale
rescale3( c(1,2,NA,3,10))



#Section 1B

##one time package install
#install.package("bio3d")

# Can you improve this analysis code?
library(bio3d)
s1 <- read.pdb("4AKE") # kinase with drug
s2 <- read.pdb("1AKE") # kinase no drug
s3 <- read.pdb("1E4Y") # kinase with drug
s1.chainA <- trim.pdb(s1, chain="A", elety="CA")
s2.chainA <- trim.pdb(s2, chain="A", elety="CA")
s3.chainA <- trim.pdb(s1, chain="A", elety="CA")
s1.b <- s1.chainA$atom$b
s2.b <- s2.chainA$atom$b
s3.b <- s3.chainA$atom$b
plotb3(s1.b, sse=s1.chainA, typ="l", ylab="Bfactor")
plotb3(s2.b, sse=s2.chainA, typ="l", ylab="Bfactor")
plotb3(s3.b, sse=s3.chainA, typ="l", ylab="Bfactor")

#use the bio3d package
library(bio3d)
s1 <- read.pdb("4AKE") # kinase with drug
s1

#fix copy/paste errors
library(bio3d)
s1 <- read.pdb("4AKE") # kinase with drug
s2 <- read.pdb("1AKE") # kinase no drug
s3 <- read.pdb("1E4Y") # kinase with drug
s1.chainA <- trim.pdb(s1, chain="A", elety="CA")
s2.chainA <- trim.pdb(s2, chain="A", elety="CA")
s3.chainA <- trim.pdb(s3, chain="A", elety="CA")
s1.b <- s1.chainA$atom$b
s2.b <- s2.chainA$atom$b
s3.b <- s3.chainA$atom$b
plotb3(s1.b, sse=s1.chainA, typ="l", ylab="Bfactor")
plotb3(s2.b, sse=s2.chainA, typ="l", ylab="Bfactor")
plotb3(s3.b, sse=s3.chainA, typ="l", ylab="Bfactor")

#test what code is actually doing
library(bio3d)

#pulling up pdb file of protein
s1 <- read.pdb("4AKE") # kinase with drug
#trimming protein to give data only from chain A and with Carbon alpha
s1.chainA <- trim.pdb(s1, chain="A", elety="CA")
#calling b factor data from the atom data in s1.chainA table
s1.b <- s1.chainA$atom$b
#plotting the b factor data on scatterplot with secondary data
plotb3(s1.b, sse= NULL, typ="l", ylab="Bfactor")


#generalizing code

library(bio3d)
#set protein code as x
x <- "4AKE"
s <- read.pdb(x)

#trim to chain A and Carbon alpha amino acid data
s.chainA <- trim.pdb(s, chain = "A", elety = "CA")

#calling b factor data from the atom data in s1.chainA tables
s.b <- s.chainA$atom$b

#plot data
plotb3(s.b, sse=s.chainA, typ="l", ylab="Bfactor")



#new function
plotbfactor <- function(x) {
  library(bio3d)
  s <- read.pdb(x)
  s.chainA <- trim.pdb(s, chain = "A", elety = "CA")
  s.b <- s.chainA$atom$b
  bf_plot <- plotb3(s.b, sse=s.chainA, typ="l", ylab="Bfactor")
  return(bf_plot)
}



