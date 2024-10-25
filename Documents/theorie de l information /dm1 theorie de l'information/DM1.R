#Création des differentes tableau 
# Création des données
categorie <- c("Agriculteursfem ", "Artisansfem", 
               "Cadresfem", "Professionsfem", 
               "Employésfem", "Ouvriersfem", "Agriculteurshom", 
               "Artisanshom", 
               "Cadreshom", 
               "Professionshom", "Employéshom", "Ouvriershom")

sexe <- c(rep("Femmes", 6), rep("Hommes", 6))
categories <- c("Agriculteurs ", "Artisans, commerçants ", 
                "Cadres", "Professions", "Employés", "Ouvriers")
age_15_29 <- c(27.8, 117.4, 564.9, 1353.7, 1570.9, 1271.6, 24.2, 79.2, 315.7, 613.3, 476.0, 1085.8)
age_30_39 <- c(70.0, 357.9, 1209.0, 1840.7, 1605.6, 1285.9, 56.2, 258.4, 685.3, 834.7, 449.5, 1068.4)
age_40_49 <- c(119.6, 556.1, 1429.5, 1895.0, 1880.9, 1362.7, 89.8, 387.2, 853.2, 915.7, 416.0, 1065.5)
age_50_59 <- c(187.1, 525.8, 1161.6, 1507.8, 1819.6, 1300.4, 138.0, 378.7, 719.0, 755.9, 329.8, 987.9)
age_60_plus <- c(76.9, 184.8, 360.0, 256.2, 397.0, 180.5, 43.4, 128.7, 240.1, 123.7, 58.3, 130.1)

# Créer le tableau
table <- data.frame(
  "Catégorie socioprofessionnelle" = categorie,
  "Sexe" = sexe,
  "De 15 à 29 ans" = age_15_29,
  "De 30 à 39 ans" = age_30_39,
  "De 40 à 49 ans" = age_40_49,
  "De 50 à 59 ans" = age_50_59,
  "60 ans ou plus" = age_60_plus
)
View(table)
ACS <- data.frame(
  "Catégorie socioprofessionnelle" = categorie,
  "De 15 à 29 ans" = age_15_29,
  "De 30 à 39 ans" = age_30_39,
  "De 40 à 49 ans" = age_40_49,
  "De 50 à 59 ans" = age_50_59,
  "60 ans ou plus" = age_60_plus
)
View(ACS)
somme <- sum(table[, sapply(table, is.numeric)])
# Créer un vecteur pour chaque colonne
categories <- c("Agriculteurs ", "Artisans, commerçants ", 
                "Cadres ", "Professions", "Employés", "Ouvriers")
femme<- c(26476.9)
homme <- c(13707.7)
x<-log10(26476.9/40184.6)*26476.9/40184.6+log10(13707.7/40184.6)*13707.7/40184.6
print(x)
# Créer une table avec deux colonnes
sexe <- data.frame("Hommes"=homme,"Femmes"=femme)
s<-sum(sexe)
# Afficher la table
print(sexe)
#creation du tbleau age 
categories <- c("Agriculteurs ", "Artisans, commerçants et chefs d'entreprises", 
                "Cadres et professions ", "Professions inter", "Employés", "Ouvriers")
age_15_29_2<- c(27.8+24.2, 117.4+79.2, 564.9+315.7, 1353.7+613.3, 1570.9+476.0,1271.6+1085.8)
age_30_39_2 <- c(126.2, 616.3, 1894.3, 2675.4,2055.1,2354.3)
age_40_49_2 <- c(209.4, 943.3, 2282.7, 1895.0+915.7, 1880.9+416, 1362.7+1065.5)
age_50_59_2 <- c(187.1+138.0, 525.8+378.7, 1161.6+719.0, 1507.8+755.9, 1819.6+329.8, 1300.4+987.9) 
age_60_plus_2 <- c(76.9+43.4, 184.8+128.7, 360.0+240.1, 256.2+123.7, 397.0+58.3, 180.5+130.1)
age <- data.frame(
  "De 15 à 29 ans" = age_15_29_2,
  "De 30 à 39 ans" = age_30_39_2,
  "De 40 à 49 ans" = age_40_49_2,
  "De 50 à 59 ans" = age_50_59_2,
  "60 ans ou plus" = age_60_plus_2)

View(age)
#creation du tableau categorie
s1=sum(age[1,])
s2=sum(age[2,])
s3=sum(age[3,])
s4=sum(age[4,])
s5=sum(age[5,])
s6=sum(age[6,])
colonne<-c(s1,s2,s3,s4,s5,s6)
View(colonne)
sum(age)
x<-data.frame(categories,colonne)
View(x)
a=sum(age[,1])
b=sum(age[,2])
c=sum(age[,3])
d=sum(age[,4])
e=sum(age[,5])
f=c(a,b,c,d,e)
sum(f)
print(f)
#calcul de hc
#D'abord calculons les entropies de chacune des variables
HC=0
for (i in c(1:6)){
  HC<-HC-sum((x[i,2])/somme)*log(sum(x[i,2]/somme))
  }
print(HC)
#calcule de lk entropie pour age
HA=0
for (i in c(1:5)){
  HA<-HA-sum((f[i])/somme)*log(sum(f[i]/somme))
}
print(HA)
#Calculer l entropîe de la variable sexe
HS<- -(log(26476.9/40184.6)*26476.9/40184.6+log(13707.7/40184.6)*13707.7/40184.6)
print(HS)
#Crezation de AC
AC<-data.frame(categories,age)
View(AC)
#Creation du tableau AS
age_15_29FH<- c(27.8+117.4+564.9+1353.7+1570.9+1271.6,24.2+79.2+315.7+613.3+476.0+1085.8)
age_30_39FH <- c(70.0+357.9+1209.0+1840.7 +1605.6+1285.9,56.2+258.4+685.3+834.7+449.5+1068.4)
age_40_49FH<- c(119.6+556.1+1429.5+1895.0+1880.9+1362.7,89.8+387.2+853.2+915.7+416+1065.5)
age_50_59FH <- c(187.1+525.8+1161.6+1507.8+1819.6+1300.4,138+378.7+719+755.9+329.8+987.9) 
age_60_plusFH<- c(76.9+184.8+360.0+256.2+397.0+180.5,43.4+128.7+240.1+123.7+58.3+130.1)
A<-data.frame(age_15_29FH,age_30_39FH,age_40_49FH,age_50_59FH,age_60_plusFH)
View(A)
S=c("F","H")
AS<-data.frame(S,A)
View(AS)
#creation du tabeau AS
femmes=c(27.8+70.0+119.6+187.1+76.9, 117.4+357.9+556.1+525.8+184.8,564.9+1209.0+1429.5+1161.6+360.0,
1353.7+1840.7+1895.0+1507.8+256.2,1570.9+1605.6+1880.9+1819.6+397.0,1271.6+1285.9+1362.7+1300.4+180.5)
hommes=c(24.2+56.2+89.8+138.0+43.4,79.2+258.4+387.2+378.7 +128.7,315.7+685.3+853.2+719.0+240.1, 613.3+
         834.7+915.7+755.9+123.7,476.0+449.5+416.0+329.8+58.3, 1085.8+1068.4+1065.5+
         987.9+130.1)
sum(hommes)
SEXE=data.frame(femmes,hommes)
CS=data.frame(SEXE,categories)
sum(CS[, sapply(CS, is.numeric)])
View(CS)
HAC=0
for (i in c(1:6)){
  for (j in 1:5) {
    HAC<-HAC-(age[i,j]/somme)*log(age[i,j]/somme)
  }
}
HAC

HAS=0
for (i in c(1,2)){
  for (j in 2:6) {
    HAS<-HAS-(AS[i,j]/somme)*log(AS[i,j]/somme)
  }
}
HAS
HCS=0
for (i in c(1,6)){
  for (j in 1:2) {
    HCS<-HCS-(CS[i,j]/somme)*log(CS[i,j]/somme)
      
  }
}
HCS
HSCA=0
for (i in c(1:12)){
  for (j in 2:6) {
    HSCA<-HSCA-(ACS[i,j]/somme)*log(ACS[i,j]/somme)
  }
}
HSCA
IAetCS=HA+HCS-HSCA
ISetAC=HS+HAC-HSCA
ICetAS=HC+HAS-HSCA
IAetCS
ISetAC
ICetAS
#l ionformation mutuelle de chaque couple
IAS=HA+HS-HAS
IAC=HA+HC-HAC
ICS=HC+HS-HCS
IAS
IAC
ICS
IA=IAS+IAC
IS=IAS+ICS
IC=IAC+ICS
IA
IS
IC
#EXERCICE2
# Créer la matrice avec les données
tableau <- matrix(c(2, 6, 8, 5, 1, 
                    27, 10, 8, 5, 0), 
                  nrow = 2, byrow = TRUE)

# Ajouter les noms des lignes et des colonnes
rownames(tableau) <- c("Oui", "Non")
colnames(tableau) <- c("0%", "Entre 0 et 0.5%", "Entre 0.5 et 1%", "Entre 1 et 3%", "> 3%")

# Afficher le tableau
View(tableau)
#4 recodage en deux classes
#tableau Z1
tableau1<-matrix(c(2,
                   6+8+5+1,27,10+8+5),
                 nrow=2,byrow=TRUE)
rownames(tableau1) <- c("Oui", "Non")
colnames(tableau1) <- c("0%",  "> 0%")
View(tableau1)
sum(tableau1)
tableaup1<-matrix(c((2+27)/72,
                    43/72),
                  nrow=1,byrow=TRUE)
rownames(tableaup1) <- c("p(j)")
colnames(tableaup1) <- c("0%",  "> 0%")
View(tableaup1)
HZ1=-((29/72)*log(29/72)+(43/72)*log(43/72))
HZ1
#tableau de Z2
tableau2<-matrix(c(8,
                   8+5+1,37,8+5),
                 nrow=2,byrow=TRUE)
rownames(tableau2) <- c("Oui", "Non")
colnames(tableau2) <- c("<=0.5%",  "> 0.5%")
View(tableau2)
#tableau avec les probas
tableaup2<-matrix(c((8+37)/72,
                   (8+5+1+8+5)/72),
                 nrow=1,byrow=TRUE)
rownames(tableaup2) <- c("p(j)")
colnames(tableaup2) <- c("<=0.5%",  "> 0.5%")
View(tableaup2)
HZ2=-((45/72)*log(45/72)+(27/72)*log(27/72))
tableau3<-matrix(c(16,
                   5+1,37+8,5),
                 nrow=2,byrow=TRUE)
rownames(tableau3) <- c("Oui", "Non")
colnames(tableau3) <- c("<=1%",  "> 1%")

View(tableau3)
#tableau des probas
tableaup3<-matrix(c(61/72,
                    11/72),
                  nrow=1,byrow=TRUE)
rownames(tableaup3) <- c("p(j)")
colnames(tableaup3) <- c("<=1%",  "> 1%")
View(tableaup3)
HZ3=-((61/72)*log(61/72)+(11/72)*log(11/72))
tableau4<-matrix(c(16+5,
                   1,37+8+5,0),
                 nrow=2,byrow=TRUE)
rownames(tableau4) <- c("Oui", "Non")
colnames(tableau4) <- c("<=3%",  "> 3%")
View(tableau4)
tableaup4<-matrix(c(71/72,
                    1/72),
                  nrow=1,byrow=TRUE)
rownames(tableaup4) <- c("p(j)")
colnames(tableaup4) <- c("<=3%",  "> 3%")
View(tableaup4)
HZ4=-((71/72)*log(71/72)+(1/72)*log(1/72))
#recodage en 3 classes
t<-matrix(c(2,6,
                   1,37+8+5,0),
                 nrow=2,byrow=TRUE)
rownames(tableau4) <- c("Oui", "Non")
colnames(tableau4) <- c("<=3%",  "> 3%")
View(tableau4)