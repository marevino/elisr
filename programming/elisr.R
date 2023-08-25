library(tidyverse)
library(xl)
library(shinythemes)
install.packages("shinythemes")
library(readxl)

# INPUT
background.file <- choose.files()
signal.file <- choose.files()
antigen <- "IFNB"
standard_highest <- 125
standard_dilutions <- 7
standard_replicates <- 2
# position of standards !

genotypes <- "C83, C86, ID"
genotypes <- as_factor(unlist(strsplit(genotypes, ", ")))
conditions <- "UT, LPS, TNF"
conditions <- as_factor(unlist(strsplit(conditions, ", ")))
data.frame(x=genotypes, y=conditions)
# load tables
background <- read_xlsx(background.file, range = "A1:L8", col_names = F)
signal <- read_xlsx(signal.file, range = "A1:L8", col_names = F)

# extract actual values
abs <- signal - background

# creating standards data frame
standard_vector <- standard_highest/(2^(0:(standard_dilutions-1)))
length(standard_vector)
standard_df <- data.frame(conc = 1:14, abs = 1:14)

# plot standard data
library(ggplot2)
standard_plot <- ggplot(standard_df, aes(x = conc, y = abs))+
  geom_point()+
  stat_smooth(method = "lm", formula = y~x, show.legend = T) +     
  labs(label=(paste(antigen, "Standard curve")),
        x = "concentration",
        y = "background corrected absorbance") +
  theme_minimal()+
  theme(text = element_text(color="black", size= 10, family="TT Arial"))
standard_plot
# extract intercept and slope from our line object
line <- lm(formula = standard_df$abs ~ standard_df$conc)
intercept <- line$coefficients[1]
slope <- line$coefficients[2]

# calculate values
Ukns <- (abs[1:8,3:12] - intercept)/slope