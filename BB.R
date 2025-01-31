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

#Taille Notes Math dans le Lyc�e A = 12: PETIT ECHANTILLON
#Variance connue = 4, sd = 2

sdMatA <- 2
xbMatA <- mean(notMatA) #moyenne empirique
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

#Taille Notes Math dans le Lyc�e B = 14: PETIT ECHANTILLON
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

#Taille Notes Math dans le Lyc�e A = 12: PETIT ECHANTILLON
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

#Taille Notes Math dans le Lyc�e B = 14: PETIT ECHANTILLON
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

# Exercice 3 ----

# 1. ----
read.csv("notes.csv") -> notes

Id <- notes$Id
math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatA <- math[lycee == 1] # "a" = 1
mean(notMatA)
sd(notMatA)
t.test(notMatA, mu = 13) # stats > TESTS > T-Test

remove(list=ls())


# 2. ----
# H0 n'�tait pas rejet�e, pval (suffisamment grande) > alpha
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


# Exercice 4 ----
# 1. ----
read.csv("notes.csv") -> notes

math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatA <- math[lycee == 1] # "a" = 1
notFrA <- franc[lycee == 1]

t.test(notMatA,notFrA,alternative = "less") # stats > TESTS > T-Test 2 ech (Group�: NON)

remove(list=ls())
# 2. ----

read.csv("notes.csv") -> notes

math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatB <- math[lycee == 2] # "b" = 2
notFrB <- franc[lycee == 2]

t.test(notMatB,notFrB,alternative = "less") 

remove(list=ls())

# 7. ----
read.csv("notes.csv") -> notes

math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatA <- math[lycee == 1] # "a" = 1
notFrA <- franc[lycee == 1]

par(mfrow=c(1,2))
qqnorm(notMatA)
abline(mean(notMatA),sd(notMatA),col=2)
qqnorm(notFrA)
abline(mean(notFrA),sd(notFrA),col=2)

remove(list=ls())
dev.off()


# 8. ----
read.csv("notes.csv") -> notes

math <- notes$math
franc <- notes$franc
lycee <- as.numeric(notes$lycee)
notMatB <- math[lycee == 2] # "b" = 2
notFrB <- franc[lycee == 2]

par(mfrow=c(1,2))
qqnorm(notMatB)
abline(mean(notMatB),sd(notMatB),col=2)
qqnorm(notFrB)
abline(mean(notFrB),sd(notFrB),col=2)

remove(list=ls())
dev.off()

# Commentaires ----
# pval: si on prend un niveau de confiance (ou risque) > que de pval% on accepte H1
# et vice versa
