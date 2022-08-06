setwd("~/Documents/Research/Density")

# import the dataset from excel

library(readxl)
wave <- read_excel("Karoilna.xlsx")

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

#create completely clean background
clean = theme(panel.grid.major = element_blank())

# histogram for cholesterol

#raw histogram
chol <- ggplot(data = cholesterol,
               aes(x = wave.chol))
chol+
  geom_histogram(color="black", fill="white")+
  theme_minimal()+
  scale_x_continuous(name = "Cholesterol")+
  scale_y_continuous("Number of observations")


#percentage histogram for cholesterol
chol.p <- ggplot(data = cholesterol,
               aes(y = (..count..)/sum(..count..),
                   x = (wave.chol)))
chol.p+
  geom_histogram(color="black", fill="white")+
  scale_x_continuous(name = "Cholesterol", limits = c(1,13), breaks = c(0:10))+
  scale_y_continuous("%",
                     labels = scales::percent_format(accuracy = NULL,
                                                     suffix = ""),
                     limits = c(0,0.20))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(size = 0.2),
        axis.line.y = element_line(size = 0.2),
        panel.grid.major.y = element_line(size = 0.3, color = "gray"))





#mean value for cholesterol
mean(cholesterol$wave.chol)
#SD for cholesterol
sd(cholesterol$wave.chol)


chol+
  geom_density()+
  theme_minimal()+
  scale_x_continuous(name = "Cholesterol", limits = c(0,13), breaks = c(0,5,10,15))+
  scale_y_continuous(name = "Density")



# histogram for HDL
# hdl <- ggplot(data = hld,
#                aes(x = wave.hdl))
# hdl+
#   geom_histogram(color="black", fill="white")+
#   theme_minimal()+
#   scale_x_continuous(name = "HDL")+
#   scale_y_continuous("Number of observations")
# 
# 
# hdl+
#   geom_density()+
#   theme_minimal()+
#   scale_x_continuous(name = "HDL")+
#   scale_y_continuous(name = "Density")


# histogram for LDL
# ldl <- ggplot(data = ldl,
#               aes(x = wave.ldl))
# ldl+
#   geom_histogram(color="black", fill="white")+
#   theme_minimal()+
#   scale_x_continuous(name = "LDL")+
#   scale_y_continuous("Number of observations")
# 
# 
# ldl+
#   geom_density()+
#   theme_minimal()+
#   scale_x_continuous(name = "LDL")+
#   scale_y_continuous(name = "Density")



# histogram for glucose
# percentage histogram for cholesterol
# glucose_hist <- ggplot(data = glucose,
#               aes(y = (..count..)/sum(..count..),
#                   x = wave.gluk))
# glucose_hist+
#   geom_histogram(color="black", fill="white")+
#   theme_minimal()+
#   scale_x_continuous(name = "Glucose", limits = c(3,12),
#                      breaks = c(4,6,8,10,12))+
#   scale_y_continuous("%",
#                      labels = scales::percent_format(accuracy = NULL, 
#                                                      suffix = ""), #suffix gives a sign after the value
#                      limits = c(0,0.20))
# 
# 
# 
# 
# glucose_hist+
#   geom_density()+
#   theme_minimal()+
#   scale_x_continuous(name = "Glucose", limits = c(3,12))+
#   scale_y_continuous(name = "Density")
# 
# 
# median(glucose$wave.gluk)
# summary(glucose$wave.gluk)


# percentage histogram for triglyceride
triglicers <- ggplot(data = triglic,
                       aes(y = (..count..)/sum(..count..),
                           x = wave.tg))
triglicers+
  geom_histogram(color="black", fill="white")+
  scale_x_continuous(name = "Triglicers [mmol/l]", limits = c(0,8),
                     breaks = c(0,1,2,3,4,5,6,7,8))+
  scale_y_continuous("%",
                     labels = scales::percent_format(accuracy = NULL,
                                                       suffix = ""), #suffix gives a sign after the value
                       limits = c(0,0.20))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(size = 0.2),
        axis.line.y = element_line(size = 0.2))

tgprop <- knitr::kable(prop.table(table(as.factor(triglic$wave.tg))))
prop.table(table(as.factor(triglic$wave.tg)))

triglic$cat[triglic$wave.tg <1] <- 0
triglic$cat[triglic$wave.tg >1 & triglic$wave.tg <= 2] <- 1
triglic$cat[triglic$wave.tg >2 & triglic$wave.tg <= 3] <- 2
triglic$cat[triglic$wave.tg >3 & triglic$wave.tg <= 4] <- 3
triglic$cat[triglic$wave.tg >4] <- 4

prop.table(table(as.factor(triglic$cat)))

order(tgprop, decreasing = TRUE)

hist(triglic$wave.tg)

#import data with smoking
waveB <- read_excel("Karolina2.xlsx")

summary(waveB$l40)

smoke <- data.frame(waveB$l40)
smoke <- na.omit(smoke)



# histogram for smoke
smoking <- ggplot(data = smoke,
                  aes(y = (..count..)/sum(..count..),
                      x = waveB.l40))
  
smoking+
  geom_histogram(color="black", fill="white")+
  scale_x_continuous(name = "Number of cigarettes", limits = c(0,40),
                     breaks = c(1:40))+
                     #expand = expansion(add = -0.01))+
  scale_y_continuous("%",
                     labels = scales::percent_format(accuracy = NULL,
                                                     suffix = ""), #suffix gives a sign after the value
                     limits = c(0,0.30))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(size = 0.2),
        axis.line.y = element_line(size = 0.2),
        axis.text.x = element_text(size = 7, angle = 45),
        panel.grid.major.y = element_line(size = 0.3, color = "gray"))



knitr::kable(prop.table(table(smoke$waveB.l40)))


#smoking in categories
smoke$cat[smoke$waveB.l40 >=0 & smoke$waveB.l40 <= 5] <- 0
smoke$cat[smoke$waveB.l40 >=6 & smoke$waveB.l40 <= 10] <- 1
smoke$cat[smoke$waveB.l40 >=11 & smoke$waveB.l40 <= 20] <- 2
smoke$cat[smoke$waveB.l40 >=21 & smoke$waveB.l40 <= 30] <- 3
smoke$cat[smoke$waveB.l40 >=30] <- 4

smoke$cat <- factor(smoke$cat,
                    levels = c(0,1,2,3,4),
                    labels = c("0-5", "6-10", "11-20", "21-30", ">30"))


smoke.cat = ggplot(data = smoke,
                   aes(y = (..count..)/sum(..count..),
                     x = cat))

smoke.cat+
  geom_bar(width = 0.5, position = "dodge2", color = "black", fill = "white")+
  scale_y_continuous("%",
                     labels = scales::percent_format(accuracy = NULL,
                                                     suffix = ""))+
  scale_x_discrete("Amount of cigarettes per day")+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(size = 0.2),
        axis.line.y = element_line(size = 0.2),
        panel.grid.major.y = element_line(size = 0.3, color = "gray"))

table(smoke$cat)
prop.table(table(smoke$cat))

#### smoking bars #####

smoking+
  geom_bar(color = "black",fill = "white", width = 0.9)+
  scale_x_continuous(name = "Number of cigarettes per day", limits = c(0,40),
                     breaks = c(1:40))+
  scale_y_continuous("%",
                     labels = scales::percent_format(accuracy = NULL,
                                                     suffix = ".0"), #suffix gives a sign after the value
                     limits = c(0,0.30))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(size = 0.2),
        axis.line.y = element_line(size = 0.2),
        axis.text.x = element_text(size = 7, angle = 45),
        panel.grid.major.y = element_line(size = 0.3, color = "gray"))




#save all plots to the directory
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="~/Documents/Research/Density")
