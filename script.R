#install.packages("dplyr")
#install.packages("MCAvariants")
#install.packages("rlang")
#install.packages("pander")
#install.packages("directlabels")
#library(rlang)
library(dplyr)
library(ggplot2)
library(gghighlight)
library(tidyr)
library(forcats)
library(pander)
library(ca)
library(psych)
library(scales)
library(gridExtra)
library(grid)
library(directlabels) 
library(ggrepel)

# FUNCTIONS #

# function handler to unfold cumulative individual observations
# into single observations
create_disjunctive_row <- function(x) {
  n <- length(x)
  nth_element <- as.numeric(x[length(x)])
  arg <- x[-n]
  unlisted <- rep(unlist(arg, use.names=F), nth_element)
  numrows <- length(unlisted) / (n - 1)
  M <- matrix(unlisted, nrow=numrows, byrow=T)
  return(M)
}

# plot 2-modal data
plot_univariate <- function(data, tit, xlbl, ylbl) {
  
  data1 <- data %>% arrange(desc(x))
  data1$Group.1 <- factor(data1$Group.1, levels=data1$Group.1)
  
  BP <- ggplot(data1, aes(x="", y=x, fill=Group.1))+
    geom_bar(position="dodge", width = 1, stat = "identity") +
    labs(title = tit, x = xlbl, y = ylbl) +
    scale_y_continuous(breaks = seq(0,max(data$x),100)) + 
    geom_text(aes(y = x, label = x, size=3), position = position_dodge(width = 1), vjust = 2, size = 4) + 
    labs(fill = "Category", x = NULL, y = NULL) 
  
  
  PCH <- ggplot(data1, aes(x="", y=x, fill=Group.1))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) + 
    geom_text(aes(label = paste(round(x / sum(x) * 100, 1), "%")), size=3.0, position = position_stack(vjust = 0.5), check_overlap = T) + 
    #geom_text_repel(aes(label = paste(round(x / sum(x) * 100, 1), "%")), size=4, box.padding = unit(0.25, "lines")) +
    labs(fill = "Category", x = NULL, y = NULL) +
    theme(legend.position = "none")
  
  grid.arrange(BP, PCH, ncol=2)  
  
}

# create univariate pander table
summarize_univarfreq_table <- function(X) {
  X %>%
    group_by(Group.1) %>%
    summarize(frequency = x) %>%
    arrange(desc(frequency)) %>%
    mutate(relative_frequency = frequency/sum(frequency),
           relative_cumulative_frequency = cumsum(relative_frequency), # %>%
           nr = row_number(-frequency)) %>%
    select(nr, Group.1, frequency, relative_frequency, relative_cumulative_frequency) %>%
    pander(split.table = 120)
}

# produce bivariate pairwise scatter plot
plot_bivariate_data <- function(data, xlbl, ylbl) {
  BCH <- ggplot(data, aes(x = Group.1, y = x, fill = Group.2, label = x)) +  
    geom_bar(stat = "identity") +                                                              
    geom_text(size = 3, position = position_stack(vjust = 0.5), check_overlap = T) + 
    labs(fill = "Category", x = xlbl, y = ylbl)
  
  BCH2 <- ggplot(data, aes(x = Group.1, y = x/sum(x), fill = Group.2, label = paste(round(x / sum(x) * 100, 1), "%"))) +  #, label = paste(round(x / sum(x) * 100, 1), "%")
    geom_bar(stat = "identity") +                                                              
    geom_text(size = 3, position = position_stack(vjust = 0.5), check_overlap = T) + 
    labs(fill = "Category", x = xlbl, y = "Relative frequency") 
  grid.arrange(BCH, BCH2, ncol=2)
}

# create frequency table
create_nvar_freq_table <- function(n, variates) {
  result <- aggregate(x = as.integer(as.character(n)), by=variates, FUN=sum)
  return(result)
}

# create bivariate pander table
summarize_2var_freq <- function(X) {
  X %>%
    group_by(Group.1, Group.2) %>%
    summarize(frequency = x) %>% 
    arrange(desc(frequency)) %>%
    pander(split.table = 120)
}

# 0.2
setwd("C:/Users/ACNXGD3ED010/Desktop/AALTO/Multivariate Statistical Analysis/project")
dv <- read.table("domestic violence.csv",header=T, sep=",")
dv <- t(dv)

# 0.3 create matrix with variables and modalities
X <- apply(dv, 1, function(x) t(unlist(strsplit(x, ";"))))

l1 <- length(dv)
d <- matrix(X, nrow=l1, byrow=T)
l2 <- length(d[1,])
dLabels <- d[1, 2:l2] #fetch colnames before removing
d <- d[-1, 1:l2] #remove colnames
d <- d[, -1] #remove year
colnames(d) <- dLabels #assign colnames to new polished data frame

DF <- data.frame(d)
Y <- DF %>% filter(Number.of.victims != 0) #filter out "empty observations

# grouped single observations 
groupedObservations <- apply(Y, 1, function(x) create_disjunctive_row(x))

# unfolded data prepared for mjca in the character form
DV.DATA <- do.call(rbind, groupedObservations)

# written as .txt
write.table(DV.DATA, file = "DV.DATA.txt", sep = "\t", row.names = F)

# 1. UNIVARIATE ANALYSIS #

# structure of data and summary statistics
str(Y)
head(Y)
glimpse(Y)
dataSummary <- summary(Y)

# collect univariate data into the table for further analysis
vSex <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.sex))
mHousing <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Mode.of.housing))
relations <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Relation.between.the.victim.and.the.suspect))
vAge <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.age))
sSex <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Suspect.s.sex))
offence <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Offence))

# 1.1 victims sex
plot_univariate(vSex, "Victim's sex distribution", "Sexes", "N.of cases")

# 1.2 mode of housing
plot_univariate(mHousing, "Household-dwelling unit's distribution", "Household type", "N.of cases")

# 1.3 relationship between victim and suspect
plot_univariate(relations, "Ratio of cases wrt Victim-Suspect relationships", "Type of relationships", "N.of cases")
summarize_univarfreq_table(relations)

# 1.4 victims age distribution
plot_univariate(vAge, "Ratio of victim's age distribution", "Age's range", "N.of cases")
summarize_univarfreq_table(vAge)

# 1.5 suspect's sex distribution
plot_univariate(sSex, "Ratio of suspect's sex distribution", "Sexes", "N.of suspects")

# 1.6 distribution of offence's types
plot_univariate(offence, "Offence's type distribution", "Offence's type", "N.of cases")
summarize_univarfreq_table(offence)

# 2.1 V.sex vs S.sex
vSex_sSex <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.sex, Y$Suspect.s.sex))
plot_bivariate_data(vSex_sSex, "Victim's sex", "N.victims")
summarize_2var_freq(vSex_sSex)

# 2.2 V.sex vs V.age
vAge_vSex <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.age, Y$Victim.s.sex))
plot_bivariate_data(vAge_vSex, "Age's range", "N.victims")
summarize_2var_freq(vAge_vSex)

# 2.3 V.age vs Relationships 
vAge_vRel <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.age,  Y$Relation.between.the.victim.and.the.suspect))
plot_bivariate_data(vAge_vRel, "Victim's age", "N.victims")
summarize_2var_freq(vAge_vRel)

# 2.4 V.age vs Offence
vAge_offence <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.age, Y$Offence))
plot_bivariate_data(vAge_offence, "Victim's age", "N.victims")
summarize_2var_freq(vAge_offence)

# 2.5 V.sex vs Relationships
vSexRel <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.sex, Y$Relation.between.the.victim.and.the.suspect))
plot_bivariate_data(vSexRel,  "Victim's sex", "N.victims")
summarize_2var_freq(vSexRel)

# 2.6 V.sex vs Offence 
vSex_offence <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.sex, Y$Offence))
plot_bivariate_data(vSex_offence, "Victim's sex", "N.victims")
summarize_2var_freq(vSex_offence)

# 2.7 V.sex vs Housing.mode
vSexHM <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.sex, Y$Mode.of.housing))
plot_bivariate_data(vSexHM, "Victim's sex", "N.victims")
summarize_2var_freq(vSexHM)

# 2.8 Housing.mode vs Relationships
vHMrel <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Mode.of.housing, Y$Relation.between.the.victim.and.the.suspect))
plot_bivariate_data(vHMrel, "Housing mode", "N.victims")
summarize_2var_freq(vHMrel)

# 2.9 Housing.mode vs Offence 
HM_offence <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Mode.of.housing, Y$Offence))
plot_bivariate_data(HM_offence, "Housing mode", "N.victims")
summarize_2var_freq(HM_offence)

# 2.10 Housing.mode vs V.age
vAgeHM <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Mode.of.housing, Y$Victim.s.age))
plot_bivariate_data(vAgeHM, "Housing mode", "N.victims")
summarize_2var_freq(vAgeHM)

# 2.11 Housing.mode vs S.sex
sSexHM <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Mode.of.housing, Y$Suspect.s.sex))
plot_bivariate_data(sSexHM, "Housing mode", "N.victims")
summarize_2var_freq(sSexHM)

# 2.12 Relationships vs S.sex
sSexRel <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Suspect.s.sex, Y$Relation.between.the.victim.and.the.suspect))
plot_bivariate_data(sSexRel, "Suspect's sex", "N.victims")
summarize_2var_freq(sSexRel)

# 2.13 Relationships vs Offence 
Rel_offence <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Relation.between.the.victim.and.the.suspect, Y$Offence))
plot_bivariate_data(Rel_offence, "Victim-Suspect Relationships", "N.victims")
summarize_2var_freq(Rel_offence)

# 2.14 V.age vs S.sex
vAge_sSex <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Victim.s.age, Y$Suspect.s.sex))
plot_bivariate_data(vAge_sSex, "Victim's age", "N.victims")
summarize_2var_freq(vAge_sSex)

# 2.15 S.sex vs Offence
{r, fig.align="center", fig.width=6, fig.height=6, fig.cap=""}
sSex_offence <- create_nvar_freq_table(Y$Number.of.victims, list(Y$Suspect.s.sex, Y$Offence))
plot_bivariate_data(sSex_offence, "Suspect's sex", "N.victims")
summarize_2var_freq(sSex_offence)


# MULTIVARIATE ANALYSIS

# 3.1 put data in the proper format for analysis
DV <- data.frame(DV.DATA)

# 3.2 perform MCA on the data
DV.MCA <- mjca(DV, lambda="indicator", reti=T)
SUMMARY <- summary(DV.MCA)

# summary information of analysis
SUMMARY

dev.off()
columns_profiles <- data.frame(SUMMARY$columns)
grid.table(columns_profiles)

# the names of MCA object 
names(DV.MCA)

# factors and levels
DV.MCA$factors

# eigenvalues of covariance matrix
DV.MCA$sv^2

# 3.4 cumulative proportion of variance explained by K principal components: 
cumVariance <- cumsum(DV.MCA$sv^2 / sum(DV.MCA$sv^2))
cumVarExpl <- data.frame(PC = paste('PC',0:22), cumvar = c(0,cumVariance))
cumVarExpl$PC <- factor(cumVarExpl$PC, levels=cumVarExpl$PC)
cumVarExpl %>% 
  ggplot(aes(x=PC,y=cumvar, group=1))+
  geom_point(size=2)+
  geom_line()+
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  labs(title="Cumulative Variance Explained by K-principal components", 
       y="Proportion of cumulative variance in %")+
  theme(legend.position = "none")


# 3.5 variance explained by each principle component:
variance_explained <- data.frame(PC = paste('PC',1:22), var_expl = SUMMARY$scree[, 3])
variance_explained$PC <- factor(variance_explained$PC, levels=variance_explained$PC)
variance_explained %>%
  ggplot(aes(x=PC, y=var_expl)) +
  geom_col() + 
  geom_text(aes(label = paste(var_expl, "%")), size=3, position = position_stack(vjust = 0.5)) +
  gghighlight(var_expl > 7.0, unhighlighted_params = list(SUMMARY$scree[, 3][3:22], colour = "steelblue")) +
  labs(title = "Variance Explained by each individual principal component (Scree plot)",
       x = "Principal Components",
       y = "% of explained variance") +
  theme_minimal()

# principle axis plots: 
plot(DV.MCA, arrows = c(T,T))
plot(DV.MCA, arrows = c(T,T), map="colgab", dim=c(1,2))
#pdf("mca.pdf", width=50, height=100)

# for pdf graphics turn off - graphics.off()
plot(DV.MCA, arrows = c(T,T), map="colprincipal")
plot(DV.MCA, arrows = c(T,T), map="rowprincipal")
plot(DV.MCA, arrows = c(T,T), map="rowgreen")

plot(ca(DV.MCA$indmat),
     arrows = c(F,T), map = 'symmetric', what = c('none','all'))
#pdf("mca2.pdf", width=50, height=100)

categories <- apply(DV, 2, function(x) nlevels(as.factor(x)))
categories

DV.MCA.DF <- data.frame(DV.MCA$colcoord, Variable=rep(names(categories), categories))
rownames(DV.MCA.DF) <- DV.MCA$levelnames
# row coordinates
DV.MCA.OBS <- data.frame(DV.MCA$rowcoord)

# plot of variable categories
ggplot(data = DV.MCA.DF, aes(x = X1, y = X2, label = rownames(DV.MCA.DF))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) +
  ggtitle("MCA plot")

# MCA plot of observations and categories
ggplot(data = DV.MCA.OBS, aes(x = X1, y = X2)) + geom_hline(yintercept = 0, 
                                                            colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_point(colour = "gray50",  alpha = 0.7) + 
  geom_density2d(colour = "gray80") + 
  geom_text(data = DV.MCA.DF, aes(x = X1, y = X2, label = rownames(DV.MCA.DF), colour = Variable)) + 
  labs(title = "MCA plot of variables",
       x = "Dimension 1 (11.8 %)",
       y = "Dimension 2 (7.5 %)") +
  scale_colour_discrete(name = "Variable")+
  theme(legend.position = "none")

#pdf("mca3.pdf", width=100, height=100)