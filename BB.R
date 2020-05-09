# Urne B

# Exercice 2 ----
# 3. ----
read.csv("notes.csv") -> notes
nconf <- 0.90

Id <- notes$Id
math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatA <- math[lycee == 1] # "a" = 1
lenMatA <- length(notMatA)

#Taille Notes Math dans le Lycée A = 12: PETIT ECHANTILLON
#Variance connue = 4, sd = 2

sdMatA <- 2
xbMatA <- mean(notMatA)
U <- qnorm(1-(1-nconf)/2)
erreurMatA <- U*sdMatA/sqrt(lenMatA)
bsupMatA <- xbMatA + erreurMatA
binfMatA <- xbMatA - erreurMatA

remove(list=ls())

# 4. ----
read.csv("notes.csv") -> notes
nconf <- 0.9

Id <- notes$Id
math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatB <- math[lycee == 2] # "b" = 2
lenMatB <- length(notMatB)

#Taille Notes Math dans le Lycée B = 14: PETIT ECHANTILLON
#Variance connue = 4, sd = 2

sdMatB <- 2
xbMatB <- mean(notMatB)
U <- qnorm(1-(1-nconf)/2)
erreurMatB <- U*sdMatB/sqrt(lenMatB)
binfMatB <- xbMatB - erreurMatB
bsupMatB <- xbMatB + erreurMatB

remove(list=ls())

# 5. ----
read.csv("notes.csv") -> notes
nconf <- 0.9

Id <- notes$Id
math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatA <- math[lycee == 1] # "a" = 1
lenMatA <- length(notMatA)

#Taille Notes Math dans le Lycée A = 12: PETIT ECHANTILLON
#Variance inconnue

T <- qt(1-(1-nconf)/2,df=lenMatA-1)
sdMatA <- sd(notMatA)
xbMatA <- mean(notMatA)
erreurMatA <- T*sdMatA/sqrt(lenMatA)
binfMatA <- xbMatA - erreurMatA
bsupMatA <- xbMatA + erreurMatA

remove(list=ls())
# 6. ----
read.csv("notes.csv") -> notes
nconf <- 0.9

Id <- notes$Id
math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatB <- math[lycee == 2] # "b" = 2
lenMatB <- length(notMatB)

#Taille Notes Math dans le Lycée B = 14: PETIT ECHANTILLON
#Variance inconnue

T <- qt(1-(1-nconf)/2,df=lenMatB-1)
sdMatB <- sd(notMatB)
xbMatB <- mean(notMatB)
erreurMatB <- T*sdMatB/sqrt(lenMatB)
binfMatB <- xbMatB - erreurMatB
bsupMatB <- xbMatB + erreurMatB

remove(list=ls())
# 7. ----
read.csv("notes.csv") -> notes
nconf <- 0.9

lycee <- as.numeric(notes$lycee)

lenLyc <- length(lycee)
lenLycA <- length(lycee[lycee==1])

F <- lenLycA/lenLyc
U <- qnorm(1-(1-nconf)/2)
erreur <- U*(sqrt((F*(1-F))/lenLyc))

#Methode 1
prop.test(lenLycA,lenLyc,conf.level=nconf)

#Methode 2
binf = F - erreur
bsup = F + erreur

remove(list=ls())
# 8. ----
read.csv("notes.csv") -> notes
nconf <- 0.9

lycee <- as.numeric(notes$lycee)

lenLyc <- length(lycee)
lenLycB <- length(lycee[lycee==2])

F <- lenLycB/lenLyc
U <- qnorm(1-(1-nconf)/2)
erreur <- U*(sqrt((F*(1-F))/lenLyc))

#Methode 1
prop.test(lenLycB,lenLyc,conf.level=nconf)
#Methode 2
binf = F - erreur
bsup = F + erreur

remove(list=ls())

# Exercice 3----

# 1. ----
read.csv("notes.csv") -> notes

Id <- notes$Id
math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatA <- math[lycee == 1] # "a" = 1

t.test(notMatA, mu = 13)

remove(list=ls())


# 2. ----
# H0 n'était pas rejetée, pval (suffisamment grande) > alpha
# 3. ----
read.csv("notes.csv") -> notes

Id <- notes$Id
math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatB <- math[lycee == 2] # "b" = 2

t.test(notMatB, mu = 13)

remove(list=ls())
# 4. ----
read.csv("notes.csv") -> notes

Id <- notes$Id
math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatB <- math[lycee == 2] # "b" = 2

t.test(notMatB, mu = 13,alternative = "less")

remove(list=ls())

