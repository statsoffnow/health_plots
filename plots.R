setwd("~/Documents/Research/Density")

# import the dataset from excel

library(readxl)
wave <- read_excel("Documents/Research/Density/Karoilna.xlsx")

# browse the column names
names(wave)

#build new datasets with only one column
cholesterol <- data.frame(wave$chol)
hld <- data.frame(wave$hdl)
ldl <- data.frame(wave$ldl)
triglic <- data.frame(wave$tg)
glucose <- data.frame(wave$gluk)


#remove NAs
cholesterol = na.omit(cholesterol)
hld = na.omit(hld)
ldl = na.omit(ldl)
triglic = na.omit(triglic)
glucose = na.omit(glucose)



#library for histograms
library(ggplot2)

# histogram for cholesterol
chol <- ggplot(data = cholesterol,
               aes(x = wave.chol))
chol+
  geom_histogram(color="black", fill="white")+
  theme_minimal()+
  scale_x_continuous(name = "Cholesterol")+
  scale_y_continuous("Number of observations")


chol+
  geom_density()+
  theme_minimal()+
  scale_x_continuous(name = "Cholesterol")+
  scale_y_continuous(name = "Density")


# histogram for HDL
hdl <- ggplot(data = hld,
               aes(x = wave.hdl))
hdl+
  geom_histogram(color="black", fill="white")+
  theme_minimal()+
  scale_x_continuous(name = "HDL")+
  scale_y_continuous("Number of observations")


hdl+
  geom_density()+
  theme_minimal()+
  scale_x_continuous(name = "HDL")+
  scale_y_continuous(name = "Density")


# histogram for LDL
ldl <- ggplot(data = ldl,
              aes(x = wave.ldl))
ldl+
  geom_histogram(color="black", fill="white")+
  theme_minimal()+
  scale_x_continuous(name = "LDL")+
  scale_y_continuous("Number of observations")


ldl+
  geom_density()+
  theme_minimal()+
  scale_x_continuous(name = "LDL")+
  scale_y_continuous(name = "Density")



# histogram for glucose
glucose_hist <- ggplot(data = glucose,
              aes(x = wave.gluk))
glucose_hist+
  geom_histogram(color="black", fill="white")+
  theme_minimal()+
  scale_x_continuous(name = "Glucose", limits = c(0,25))+
  scale_y_continuous("Number of observations")


glucose_hist+
  geom_density()+
  theme_minimal()+
  scale_x_continuous(name = "Glucose", limits = c(0,25))+
  scale_y_continuous(name = "Density")


# histogram for triglyceride
triglicers <- ggplot(data = triglic,
                       aes(x = wave.tg))
triglicers+
  geom_histogram(color="black", fill="white")+
  theme_minimal()+
  scale_x_continuous(name = "Triglyceride")+
  scale_y_continuous("Number of observations")


triglicers+
  geom_density()+
  theme_minimal()+
  scale_x_continuous(name = "Triglyceride")+
  scale_y_continuous(name = "Density")


#import data with smoking
waveB <- read_excel("Documents/Research/Density/Karolina2.xlsx")

summary(waveB$l40)

smoke <- data.frame(waveB$l40)
smoke <- na.omit(smoke)

# histogram for smoke
smoking <- ggplot(data = smoke,
                  aes(x = waveB.l40))

  
smoking+
  geom_histogram(color="black", fill="white")+
  theme_minimal()+
  scale_x_continuous(name = "Number of cigarettes")+
  scale_y_continuous("Number of observations")


smoking+
  geom_density()+
  theme_minimal()+
  scale_x_continuous(name = "Number of cigarettes")+
  scale_y_continuous(name = "Density")


#save all plots to the directory
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to="~/Documents/Research/Density")
