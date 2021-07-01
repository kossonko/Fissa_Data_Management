###################GESTION_DE_DONNEES_ALERRT########################
library(readr)
library(dplyr)
alerrt<- read_csv("C:/Users/LP-10-ALERRT-05/Desktop/ALERRT/ALERRTFISSASTUDYCote_DATA_2020-11-02_1805.csv")
head(alerrt[,1:5])
alerrt_etude <- filter(alerrt,is.na(redcap_repeat_instrument)==TRUE)
head(alerrt_etude[,1:5])

########ELIGIBIITE##############################
eligibilite <- select(alerrt_etude,contains("eli"))
head(eligibilite[,1:5])
cat("il y a", length(which(is.na(eligibilite))), "donnees manquantes dans le formulaire inclusion.")



######################INCLUSION##################################
inclusion <- select(alerrt_etude,contains(c("q1_eli","inc","q2_inc")))
#Donn�es manquantes
cat("il y a", length(which(is.na(inclusion))), "donnees manquantes dans le formulaire inclusion.")
#Ord0nance apres consultaion et hospitalisation
inclusion_ord<- filter(inclusion, (q27_inc =="2") & (q28_inc=="1"))
cat("les patients ayant ete hospitalise et ayant recu une ordonnance sont :",inclusion_ord$q1_eli)



######################VISITE_J-7#################################
visite_j7 <- select(alerrt_etude,contains(c("q1_eli","schd_d7","suivi_j7","q2_inc")))
#Donn�es manquantes
cat("il y a", length(which(is.na(visite_j7))), "donn�es manquantes dans le formulaire J-7.")
#Requete sur les dates de visites de j-7
visite_j7$q2_inc <-as.Date(visite_j7$q2_inc,"%d/%m/%Y")
visite_j7$q1a_schd_d7 <-as.Date(visite_j7$q1a_schd_d7,"%d/%m/%Y")
requete_date_7 <-filter(visite_j7,difftime(q1a_schd_d7,q2_inc ,units = "days")> 9)
cat("les patients dont la visite J-7 est eronn� sont:",requete_date_7$q1_eli)



######################VISITE_J-14################################
visite_j14 <- select(alerrt_etude,contains(c("q1_eli","schd_d14","suivi_j14","q2_inc")))
#Donn�es manquantes
cat("il y a", length(which(is.na(visite_j14))), "donn�es manquantes dans le formulaire J-14.")
#Requete sur les dates de visites de j-14
visite_j14$q2_inc <-as.Date(visite_j14$q2_inc,"%d/%m/%Y")
visite_j14$q1a_schd_d14 <-as.Date(visite_j14$q1a_schd_d14,"%d/%m/%Y")
requete_date_14 <-filter(visite_j14,difftime(q1a_schd_d14,q2_inc , units = "days")> 16 )
cat("les patients dont la visite J-14 est eronn� sont:",requete_date_14$q1_eli)


###################### VISITE_J-21 ################################
visite_j21 <- select(alerrt_etude,contains(c("q1_eli","schd_d21","suivi_j21","q2_inc")))
#Donn�es manquantes
cat("il y a", length(which(is.na(visite_j21))), "donn�es manquantes dans le formulaire J-21.")
#Requete sur les dates de visites de j-21
visite_j21$q2_inc <-as.Date(visite_j7$q2_inc,"%d/%m/%Y")
visite_j21$q1a_schd_d21 <-as.Date(visite_j21$q1a_schd_d21,"%d/%m/%Y")
requete_date_21 <-filter(visite_j21,difftime(q1a_schd_d21,q2_inc, units = "days")> 23 )
cat("les patients dont la visite J-21 est eronn� sont:",requete_date_21$q1_eli)
#Requete sur nombre de jours d'apyrexie superieur a 9 a J-21
visite_j21$q6b_schd_d21 <- as.numeric(visite_j21$q6b_schd_d21)
apyrexie <- filter(visite_j21, q6b_schd_d21 > 9)
cat("Les patients dont le nombre de jours d'apyrexie a J-21 est superieure a 9 sont:",apyrexie$q1_eli)




######################CONCLUSION#################################

######################FORMULAIRE_TRAITEMENT######################
traitement <- filter(alerrt,redcap_repeat_instrument== "formulaire_traitement")
formulaire_traitement <- select(traitement,contains(c("q1_eli","trmt","trait")))


######################EXAMENS_LABORATOIRE########################
examen <- filter(alerrt,redcap_repeat_instrument=="examen_labo")
examen_labo <- select(examen,contains(c("q1_eli","lab","inst")))

######################VISITES_IMPREVUES##########################


