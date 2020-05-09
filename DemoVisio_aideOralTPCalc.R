

#quelques questions de proba
#========================================
#probas et quantiles avec loi normale mu=2 sigma=3
pnorm(4,2,3)-pnorm(0,2,3)#P(X in [0,4])
pnorm(4,2,3)-pnorm(-4,2,3)#P(X in [-4,4])
qnorm(0.8,2,3)#quantile d'ordre 0.8 attention avec la calculette par défaut tail=left (surface de 0.8 à qauche du quantile)
#------------------------------------------
#probas et quantiles avec loi du chi-deux à 3 dl
pchisq(4,3)-pchisq(0,3)#P(X in [0,4])
qchisq(0.8,3)#quantile d'ordre 0.8. 
#attention avec les calculettes l'option tail=right signifie pour le chideux surface à gauche=0.8
#je ne comprends pas cette logique inversée par rapport à la loi normale mais c'est ainsi
#sur la plupart des TI la fonction qunatile n'est même pas proposée donc dans ce cas si pas de R alors lire les tables
#------------------------------------------
#probas et quantiles avec loi de Student  à 3 dl
pt(4,3)-pt(0,3)#P(X in [0,4])
qt(0.8,3)#quantile d'ordre 0.8. 
#attention avec les calculettes l'option tail=right signifie pour le chideux surface à gauche=0.8. donc meme resultat qu'avec qt
#je ne comprends pas cette logique inversée par rapport à la loi normale mais c'est ainsi
#sur la plupart des TI la fonction qunatile n'est même pas proposée donc dans ce cas si pas de R alors lire les tables

#quelques questions de stat
#===========================================================================================================

#les donnees proposées au départ
#-----------------------------------------------------------------------------------------------------------
read.csv("notes.csv")->not   #charge les données et crée le dat.frame not
#Vew(not)                     #visualisation des données en tableau
not                          #affichage du data.frame dans la fenêtre de commandes (en bas)
math<-not$math               #extraction de l'échantillon des notes de maths et affectation dans math
franc<-not$franc             #idem pourles notes de français
lycee<-as.numeric(not$lycee) #crée l'échantillon lycée où on remplace la lettre a par le code numérique 1 et la lettre b par le code numérique 2
lycee


#les données proposées au départ avec un individu supplémentaire (27,m,f,l)
#------------------------------------------------------------------------------------------------------------
#si on enrichit l'écahntillon des 26 candidat par l'ajout d'un candidat suplementaire 
#(27,m,f,l)=(27,15,17,a) qui a eu 15 en mths 17 en français et vient du lycée A il faut exécuter les lignes suivantes
#avant de fabriquer les echantillons extraits avec R. Avec la calculette saisir les valeurs supplémentaires dans les 
#listes adéquates : dans ce cas puisque lycée A, m dans liste 1 en 13ieme ligne, f dans liste 2 en 15 ieme ligne et avec R
#executer les 4 lignes suivantes pour mettre à jour les echantillons 

#m=15;f=17;l=1             # le lycée A est codé numériquement par la valeur 1 
#math<-c(math,m)           # rajoute la note de math de l'inidv num27 à l'echantillon des notes de maths de départ
#franc<-c(franc,f)         # rajoute la note de français de l'inidv num27 à l'echantillon des notes de français de départ
#lycee<-c(lycee,1)

  

lycee[lycee==1]                #échantillon du lycée d'origine quand on vient du lycée A
length(lycee[lycee==1])->na    #nombre de candidats de l'échantillon qui viennent de A
length(lycee)->n               # taille de l'échantillon prélevé dans la population reunissant tous les terminales des deux lycées

#les echantillons extraits à saisir dans les listes des calculettes ou à extraire sous R
#-------------------------------------------------------------------------------------------------------------

#les notes de maths dans les deux lycées
ma<-math[lycee==1]          #ech des notes de math dans le lycée A ==> à saisir dans la liste 1 des calculettes
mb<-math[lycee==2]          #ech des notes de math dans le lycée B ==> à saisir dans la liste 3 des calculettes


#les notes de francais dans les deux lycées
fa<-franc[lycee==1]        #ech des notes de français dans le lycée A ==> a saisir dans la liste 2 des calculettes 
fb<--franc[lycee==2]       #les notes de français dans le lycée B ==> a saisir dans la liste 4 des calculettes


#les résumes numeriques de base des echantillons
#------------------------------------------------

summary(ma);sd(ma)        #math lycée A (liste 1 calculette)
summary(mb);sd(mb)        #math lycée B (liste 3 calculette)
summary(fa);sd(fa)        #français lycée A (liste 1 calculette)
summary(fb);sd(fb)        #français lycée B (liste 3 calculette)

#Inférence statistique
#==================================================================

#notations  : XA va qui modélise la note de maths lycée A de moyenne muXA et ecart-type sigmaXA à priori tous deux inconnus
#             XB va qui modélise la note de maths lycée B de moyenne muXB et ecart-type sigmaXB à priori tous deux inconnus
#             pA probabilité inconnue qu'un candidat choisi au hasard dans les deux lycée vienne de A

#Estimation de muXA et sigmaXA
#-----------------------------
muXAest<-mean(ma)           #esb de muX qu'on peut aussi lire dans la sortie du summary
sigmaXAest<-sd(ma)         #esb de sigmaX

#Intervalles de confiance sur muXA de niveau 1-alpha
#---------------------------------------------------
#hypothèse requise (hypothèse de modélisation) surtout si petit echantillon : XA de loi normale

#Si sigmaXA connu et de valeur donnée : calculs programmés à la main :
alpha<-0.2
mean(ma)-2*qnorm(1-alpha/2)/sqrt(length(ma))
mean(ma)+2*qnorm(1-alpha/2)/sqrt(length(ma))
# sigma_X inconnu c'est plus rapide car t.test fait le calcul :
t.test(ma,conf.level=1-alpha)

#Intervalle de confiance sur une proportion de niveau 1-alpha
#-----------------------------------------------------------

#pour l'intervalle asymptotique du cours faire le calcul à la main et sous condition echantillon de taille suffisnate
#on verifie n*pest>10 et n*(1-pest)>10
pAest<-na/n
alpha<-0.2
u<-qnorm(1-alpha/2)
pest-u*sqrt(pAest*(1-pAest)/n)
pest+u*sqrt(pAest*(1-pAest)/n)

#ou avec la procédure prop.test qui produit un intervalle différent de celui du cours ou de la calculette
#c'est normal car comme déjà vu prop.test ne reproduit pas exactement l'IC vu en cours par contre la calculette=IC du cours
#--------------------------------------------------------------------------------------------------------------------------
prop.test(na,n,conf.level=1-alpha)


# Test de comparaison des moyennes muXA et muXB avec échantillons indépendants

# hypothèses requises (de modélisation)
# XA math lycee A moyenne muXA ecart-type sigmaXA de loi normale
# XB math lycee B moyenne muXB ecart-type sigmaXB de loi normale
# sigmaXA=sigmaXB car petits echantillons


#si test bilatéral :
#H0 : muXA-muXB=0 H1: muXA-muXB diff 0
#hyp de modelisation pour comp de moy avec ech indep et petit : normalite de X et de Y et sigX=sigY
 
t.test(ma,mb,mu=0,paired=F,var.equal=T)#on ne peut valider H1 de façon signif car pval trop gde

