# -----------------------------------------------------
# Generalized Linear Model (GLM) for pump complications
# -----------------------------------------------------

library(plyr)

# clean up
rm(list = ls())

setwd('C:/Documenten/Statistiek/Baclophen/')
setwd('G:/Baclophen/')

# read file with complications; one row per patiant, years in columns
comp.file <- read.table('complicaties_all.txt', header = TRUE, sep = '\t')

# convert the complication file in a data frame in which each patient-year is a row
complicaties <- data.frame()
for (i in 1:22) {
  pompjaar.data <- comp.file[,c('patientnr', paste0('X',i))]
  names(pompjaar.data)[2] <- 'ncomp'
  pompjaar.data <- cbind(pompjaar.data, pompjaar = i)
  pompjaar.data <- pompjaar.data[!is.na(pompjaar.data$ncomp),]
  complicaties <- rbind(complicaties, pompjaar.data)
}


# read patient data
patienten <- read.table('patienten.txt', header = TRUE, sep = '\t')

# merge complication and patient data
complicaties <- merge(patienten, complicaties, c("patientnr"))
# re-shuffle columns
complicaties <- complicaties[, c("patientnr", "geslacht", "aandoening", "instaljaar", "pompjaar", "ncomp")]

# write the data frame to a file
write.table(complicaties, 'complicaties_dataframe.txt', sep = '\t', row.names = FALSE, quote = FALSE)

# ---------
# analysis
# ---------

glm1 <- glm(data = complicaties, ncomp ~ 1, family = poisson)
frequency <- exp(glm1$coefficients[1])
sum(complicaties$ncomp)/NROW(complicaties)

glm.geslacht <- glm(data = complicaties, ncomp ~ geslacht, family = poisson)
summary(glm.geslacht)
exp(glm.geslacht$coefficients)
sum(complicaties$ncomp[complicaties$geslacht == 'M'])/NROW(complicaties[complicaties$geslacht == 'M',])
sum(complicaties$ncomp[complicaties$geslacht == 'V'])/NROW(complicaties[complicaties$geslacht == 'V',])
# significant verschil tussen mannen en vrouwen 20% vs 12%

glm.aandoening <- glm(data = complicaties, ncomp ~ aandoening, family = poisson)
summary(glm.aandoening)
exp(glm.aandoening$coefficients)
sum(complicaties$ncomp[complicaties$aandoening == 'MS'])/NROW(complicaties[complicaties$aandoening == 'MS',])
sum(complicaties$ncomp[complicaties$aandoening == 'SCI'])/NROW(complicaties[complicaties$aandoening == 'SCI',])
# geen significant verschil tussen MS en SCI 15% vs 19%

glm.pompjaar <- glm(data = complicaties, ncomp ~ factor(pompjaar), family = poisson)
summary(glm.pompjaar)
exp(glm.pompjaar$coefficients)
# geen significante trend in functie van het pompjaar

glm.pompjaar <- glm(data = complicaties, ncomp ~ pompjaar, family = poisson)
summary(glm.pompjaar)
exp(glm.pompjaar$coefficients)
ddply(complicaties, .(pompjaar), summarize, risico = sum(ncomp)/NROW(ncomp))
# de eerste 4 pompjaren zijn risicovoller

