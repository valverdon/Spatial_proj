###############################################################
# ENVdata
#Get topoclimatic data from different sources                            #
# extract values at sampling points                           #
# merge everything in Envdata_full                            #
#     #
#                TODO : change with 00_Prep in SESAM files 18/03/24 (better workflow)
###############################################################
#rm(list=ls())
library(terra)
library(tidyverse)
library(Tjazi)
##################################### climatic and topographic variables ####################################################

ENVstack<-readRDS(file="../../spatial_data/ENVstack.rds")
ENVstack<-terra::unwrap(ENVstack)
# names(ENVstack)
# plot(ENVstack[["Altitude"]])

#problem : 5 points out of Altitude layer

########################################## edaphic variables #####################################################################
#point data
# load OTU data from 265 sites in addition to soil data to find matching sites
load("PA\\BA\\data\\BA_OTU_265samples.Rdata")
# dim(OTU_data_bac_265samples)
# colnames(OTU_data_bac_265samples)
# load("PA\\FU\\data\\FU_OTU_232samples.Rdata")
# load("PA\\PR\\data\\PR_OTU_179samples.Rdata")

load("../../spatial_data/MpAlps_soil_data_VVerdon1903+.Rda")
edaph_data_BA <- dataSoil[dataSoil$sampleNameBA %in% colnames(OTU_data_bac_265samples), ] # pick only sites for which there is BA OTUdata
# edaph_data_FU <- dataSoil[dataSoil$sampleNameFU %in% colnames(OTU_data_fun_232samples), ] # pick only sites for which there is FU OTUdata
# edaph_data_PR <- dataSoil[dataSoil$sampleNamePR %in% colnames(OTU_data_pro_179samples), ] # pick only sites for which there is PR OTUdata
# edaph_data_BA$altitude
ENVdatatemp_BA <- terra::extract(ENVstack, edaph_data_BA[,c("x","y")])
# from_raster_FU <- terra::extract(ENVstack, edaph_data_FU[,c("x","y")])
# from_raster_PR <- terra::extract(ENVstack, edaph_data_PR[,c("x","y")])

# plot(ENVstack$bio1_t)
# points(edaph_data_BA[which(edaph_data_BA$sampleNameBA%in%rownames(OTUdata_BA_PA)),c("x","y")],col="red")
# points(edaph_data_FU[which(edaph_data_FU$sampleNameFU%in%rownames(OTUdata_BA_FU)),c("x","y")],col="red")
# points(edaph_data_PR[which(edaph_data_PR$sampleNamePR%in%rownames(OTUdata_BA_PR)),c("x","y")],col="red")

# ENVdatatemp_BA <- data.frame(from_raster_BA[,colnames(from_raster_BA)!="d13C"], #for stack with d13C that double the var
#                              edaph_data_BA[,c(13:ncol(edaph_data_BA))],
#                              AllSand=edaph_data_BA$ThickSand+edaph_data_BA$ThinSand, 
#                              AllSilt=edaph_data_BA$ThickSilt+edaph_data_BA$ThinSilt)
# ENVdatatemp_FU <- data.frame(from_raster_FU[,colnames(from_raster_FU)!="d13C"], 
#                              edaph_data_FU[,c(13:ncol(edaph_data_FU))],
#                              AllSand=edaph_data_FU$ThickSand+edaph_data_FU$ThinSand, 
#                              AllSilt=edaph_data_FU$ThickSilt+edaph_data_FU$ThinSilt)
# ENVdatatemp_PR <- data.frame(from_raster_PR[,colnames(from_raster_PR)!="d13C"], 
#                              edaph_data_PR[,c(13:ncol(edaph_data_PR))],
#                              AllSand=edaph_data_PR$ThickSand+edaph_data_PR$ThinSand, 
# AllSilt=edaph_data_PR$ThickSilt+edaph_data_PR$ThinSilt)
# ENVdatatemp_BA <- data.frame(from_raster_BA,
#                              edaph_data_BA[,c(13:ncol(edaph_data_BA))],
#                              AllSand=edaph_data_BA$ThickSand+edaph_data_BA$ThinSand,
#                              AllSilt=edaph_data_BA$ThickSilt+edaph_data_BA$ThinSilt)
# ENVdatatemp_BA$Limestones_SandyLimestones_.Marl_Shale

# summary(ENVdatatemp_BA) ; nrow(ENVdatatemp_BA);ncol(ENVdatatemp_BA)
#remove some var 
#c("drySoilWeight","wetSoilWeight","EC_1_1","AllSand","AllSilt")
#dry and wet because they are just here to have water content (multicol)
#EC because we have 2 ways of measuring the same thing
#AllSand and Allsilt because they are sum of more precise ones already in the table
#removed Ankerite, because "04_Clust_UnivMod.R" cant fit univariate gam model for OTU15660 for that variable. And its not an important variable.
#removed Dolomite, because not enough values != from 0 to perform variable selection with cubic regression.
#(later classified in "trash")
# varcor <- cor(ENVdatatemp_BA,use = "pairwise.complete.obs")
# plot(hclust(as.dist(1-abs(var_cor)), "average"))
# abline(h=0.3,lty=2, col="red", lwd=2)
# abline(h=0.2,lty=2, col="black", lwd=2)
# abline(h=0.25,lty=2, col="blue", lwd=2)

#Tmax removed, not interpretable and missing data
#C.N removed : Obvious multicolinearity (literally Carbon / Nitrogen) and missing data
#TOC removed : missing data and highly correlated with Nitrogen/Carbo /BSWC group
#MINC removed : missing data and highly correlated with CaO/pH group
#Phyllosilicates + Quartz Feldspath_K Plagioclase_Na    Calcite Goethite   Indoses : not very informative (except calcite but correlated), win more plots.
ENVdatatemp_BA <- ENVdatatemp_BA[,-which(names(ENVdatatemp_BA)%in%c("ID","north1"))]

#remove HI OI cause too many empty data

ENVdatatemp_BA <- ENVdatatemp_BA[,-which(names(ENVdatatemp_BA)%in%c("AngularGravel_Blocks_SlopeScree","Conglomerates_weakmed_solidified_with_sandstone_and_deposits","Dolomites_Cornelia","GravelSand_currentdeposit","Dolomite","Goethite","Ankerite","Limestones_SandyLimestones_.Marl_Shale"))]

rownames(ENVdatatemp_BA) <- edaph_data_BA$sampleNameBA
#Remove sites with NA
#nrow(ENVdatatemp_BA)
# tail(ENVdatatemp_BA[!(complete.cases(ENVdatatemp_BA)),])
# summary(ENVdatatemp_BA[!(complete.cases(ENVdatatemp_BA)),])
ENVdatatemp_BA <- ENVdatatemp_BA[complete.cases(ENVdatatemp_BA),] #264 -> 250
# summary(ENVdatatemp_BA) ; nrow(ENVdatatemp_BA)

#remove binary var with not enough points with the var

#factorization of remaining binary var
binvar <- c()
for (i in names(ENVdatatemp_BA)){#i =names(ENVdata) [31]
  if(length(unique(ENVdatatemp_BA[,i]))==2){
    ENVdatatemp_BA[,i]<-as.factor(ENVdatatemp_BA[,i])
    binvar <- c(binvar,i)
  }
}
# summary(ENVdatatemp_BA)
# nrow(ENVdatatemp_BA) 250
# ncol(ENVdatatemp_BA) 77 dont 8 bin
ENVdata_BA <-ENVdatatemp_BA


### Only to have an idea about taxonomy in data filtering steps
load("../../../30_data/ASVtables_taxo/BAtaxo2023_full.Rda")

# EUKtaxofull<-euk_silva.tax
# PRtaxofull<-read.csv("../../../30_data/ASVtables_taxo/Tax_table_Protists.csv",sep=",")



#ASV data preparation
#Bacteria
OTUdata_BA_AB <- OTU_data_bac_265samples[,which(colnames(OTU_data_bac_265samples)%in%dataSoil$sampleNameBA)]

# load(paste0("../../ASV_data/ASV_taxo/BAtaxo.Rda"))
OTUdata_BA_AB_taxo<-cbind(OTUdata_BA_AB ,BAtaxo2019[match(rownames(OTUdata_BA_AB),BAtaxo2019[,"Seq"]),])
all(OTUdata_BA_AB_taxo$Seq==rownames(OTUdata_BA_AB_taxo))

OTUdata_BA_AB <- t(OTUdata_BA_AB)
# nrow(OTUdata_BA_AB) #264
# ncol(OTUdata_BA_AB) #60567 ASV
# sum(OTUdata_BA_AB_taxo$Kingdom=="Bacteria",na.rm = TRUE) 52177 Bacteria
# sum(OTUdata_BA_AB_taxo$Kingdom=="Archaea",na.rm = TRUE)  204 Archaea
# sum(OTUdata_BA_AB_taxo$Kingdom=="unclassified_Root",na.rm = TRUE) 8186

#length(complete.cases(ENVdatatemp_BA))
#remove plots that dont have measures for all variables




OTUdata_BA_AB <- OTUdata_BA_AB[rownames(ENVdata_BA), ] 
# nrow(OTUdata_BA_AB) #264

OTUdata_BA_AB <- OTUdata_BA_AB[,colSums(OTUdata_BA_AB)>99 ]# the selection of plots made some OTU to go under the threshold of 100 total read counts, remove them
#ncol(OTUdata_BA_AB) #60534
OTUdata_BA_AB_taxo2<-BAtaxo2019[match(colnames(OTUdata_BA_AB),BAtaxo2019[,"Seq"]),]
all(OTUdata_BA_AB_taxo2$Seq==colnames(OTUdata_BA_AB))
# sum(OTUdata_BA_AB_taxo2$Kingdom=="Bacteria",na.rm = TRUE) 52149 Bacteria
# sum(OTUdata_BA_AB_taxo2$Kingdom=="Archaea",na.rm = TRUE)  204 Archaea
# sum(OTUdata_BA_AB_taxo2$Kingdom=="unclassified_Root",na.rm = TRUE)  8181 ??

#easier datset for future (remove unassigned taxa directly instead of post-modelling):
OTUdata_BA_AB <- OTUdata_BA_AB[,OTUdata_BA_AB_taxo2$Kingdom!="unclassified_Root"]
OTUdata_BA_AB_taxo2<-BAtaxo2019[match(colnames(OTUdata_BA_AB),BAtaxo2019[,"Seq"]),]

#Need treshold to remove very high and very low prevalence (algos wont be able to adjust to data)
#5 or 10% ?
OTUdata_BA_AB_all<-OTUdata_BA_AB
numberpresence_BA <- apply(OTUdata_BA_AB_all,2,function(X){sum(X!=0)})
# plot(numberpresence_BA,pch=20,cex=.5, main="number of plots in which the ASV \n is present")
# # abline(h=nrow(OTUdata_BA_AB)/10,col="blue")#10% cut
# abline(h=nrow(OTUdata_BA_AB)/20,col="red") #5% cut
# # abline(h=nrow(OTUdata_BA_AB)-(nrow(OTUdata_BA_AB)/10),col="blue")#10% cut
# abline(h=nrow(OTUdata_BA_AB)-(nrow(OTUdata_BA_AB)/20),col="red") #5% cut
# plot(density(numberpresence_BA))

# # abline(v=nrow(OTUdata_BA_AB)/10,col="blue")#10% cut
# abline(v=nrow(OTUdata_BA_AB)/20,col="red")#5% cut
# # abline(v=nrow(OTUdata_BA_AB)-(nrow(OTUdata_BA_AB)/10),col="blue")#10% cut
# abline(v=nrow(OTUdata_BA_AB)-(nrow(OTUdata_BA_AB)/20),col="red") #5% cut
# densityx<-lapply(density(numberpresence_BA)$x,function(X){ifelse(X>nrow(OTUdata_BA_AB)/20,nrow(OTUdata_BA_AB)/20,X)})
# polygon(densityx,density(numberpresence_BA)$y,col="grey")
# densityx2<-lapply(density(numberpresence_BA)$x,function(X){ifelse(X<nrow(OTUdata_BA_AB)-(nrow(OTUdata_BA_AB)/20),nrow(OTUdata_BA_AB)-(nrow(OTUdata_BA_AB)/20),X)})
# polygon(densityx2,density(numberpresence_BA)$y,col="grey")

removed_toomuchrare <- sum(numberpresence_BA<nrow(OTUdata_BA_AB)/20)
removed_toogeneral <- sum(numberpresence_BA>nrow(OTUdata_BA_AB)-nrow(OTUdata_BA_AB)/20)
removed_toorare_taxo<-OTUdata_BA_AB_taxo2[numberpresence_BA<nrow(OTUdata_BA_AB)/20,]
removed_toogeneral_taxo<-OTUdata_BA_AB_taxo2[numberpresence_BA>nrow(OTUdata_BA_AB)-nrow(OTUdata_BA_AB)/20,]

# removed_toomuchrare 2439
# removed_toogeneral 809   #they are only removed in PA models
# removed_toomuchrare+removed_toogeneral #3248
sum(removed_toorare_taxo$Kingdom=="Bacteria",na.rm = TRUE)#2405
sum(removed_toorare_taxo$Kingdom=="Archaea",na.rm = TRUE)#34
sum(removed_toorare_taxo$Kingdom=="unclassified_Root",na.rm = TRUE)# 0    if 0 -> removal ok
sum(removed_toogeneral_taxo$Kingdom=="Bacteria",na.rm = TRUE)#809
sum(removed_toogeneral_taxo$Kingdom=="Archaea",na.rm = TRUE)#0
sum(removed_toogeneral_taxo$Kingdom=="unclassified_Root",na.rm = TRUE)#0  if 0 -> removal ok


#remove less prevalent ones for AB
OTUdata_BA_AB <- OTUdata_BA_AB_all[,numberpresence_BA>=nrow(OTUdata_BA_AB_all)/20]
OTUdata_BA_AB_taxo2<-BAtaxo2019[match(colnames(OTUdata_BA_AB),BAtaxo2019[,"Seq"]),]

#transformation AB --> PA
OTUdata_BA_PA <- apply(OTUdata_BA_AB,c(1,2),FUN=function(X){X>0}) 
OTUdata_BA_PA2 <- OTUdata_BA_AB_all[,as.logical((numberpresence_BA>=nrow(OTUdata_BA_AB_all)/20)*(numberpresence_BA<nrow(OTUdata_BA_AB_all)-nrow(OTUdata_BA_AB_all)/20))]

OTUdata_BA_PA2 <- apply(OTUdata_BA_PA2,c(1,2),FUN=function(X){X>0}) 

# install.packages("remotes")
# remotes::install_github("thomazbastiaanssen/Tjazi")

t1<-Sys.time()
OTUdata_BA_AB_clr <- clr_unif(OTUdata_BA_AB,samples_are ="rows")
Sys.time()-t1 #35min

# hist(OTUdata_BA_AB_clr[,1])
seqvec_ABdata <- colnames(OTUdata_BA_AB)
names(seqvec_ABdata) <- paste0("OTU",1:ncol(OTUdata_BA_AB))
all(OTUdata_BA_AB_taxo2$Seq==seqvec_ABdata)
# saveRDS(seqvec_ABdata,file="spat_exp_PA/seqvecBA.Rds")

# ncol(OTUdata_BA_PA)# 49914 ASV
# ncol(OTUdata_BA_PA2)# 49105 ASV
#253arrays of 220 ASV
TotSeqSum_BA <- rowSums(OTUdata_BA_AB)




# #################################Fungi
# 
# OTUdata_FU_AB <- OTU_data_fun_232samples[,which(colnames(OTU_data_fun_232samples)%in%dataSoil$sampleNameFU)]
# OTUdata_FU_AB_taxo<-cbind(OTUdata_FU_AB ,FUtaxo2021[match(rownames(OTUdata_FU_AB),FUtaxo2021[,"Seq"]),])
# all(OTUdata_FU_AB_taxo$Seq==rownames(OTUdata_FU_AB_taxo))
# 
# OTUdata_FU_AB <- t(OTUdata_FU_AB)
# # nrow(OTUdata_FU_AB) #232
# #ncol(OTUdata_FU_AB) #95387
# # sum(OTUdata_FU_AB_taxo$Kingdom=="Fungi",na.rm = TRUE) 31112 Fungi
# # sum(OTUdata_FU_AB_taxo$Kingdom!="Fungi",na.rm = TRUE) 64261 not Fungi (or unclassified)
# # sum(is.na(OTUdata_FU_AB_taxo$Kingdom)) 0
# 
# OTUdata_FU_AB <- OTUdata_FU_AB[rownames(ENVdata_FU), ] # #remove plots that dont have measures for all variables
# # nrow(OTUdata_FU_AB) #217
# OTUdata_FU_AB <- OTUdata_FU_AB[,colSums(OTUdata_FU_AB)>99 ]# the selection of plots made some OTU to go under the threshold of 100 total read counts, remove them
# 
# # ncol(OTUdata_FU_AB) #92753
# OTUdata_FU_AB_taxo2<-FUtaxo2021[match(colnames(OTUdata_FU_AB),FUtaxo2021[,"Seq"]),]
# all(OTUdata_FU_AB_taxo2$Seq==colnames(OTUdata_FU_AB))
# 
# # sum(OTUdata_FU_AB_taxo2$Kingdom=="Fungi",na.rm = TRUE)# 30366 Fungi
# # sum(OTUdata_FU_AB_taxo2$Kingdom!="Fungi",na.rm = TRUE)# 0 is removal above # 62387 not Fungi (or unclassified)
# # sum(is.na(OTUdata_FU_AB_taxo2$Kingdom))# 0
# 
# #easier datset for future (remove unassigned taxa directly instead of post-modelling):
# OTUdata_FU_AB <- OTUdata_FU_AB[,OTUdata_FU_AB_taxo2$Kingdom=="Fungi"]
# OTUdata_FU_AB_taxo2<-FUtaxo2021[match(colnames(OTUdata_FU_AB),FUtaxo2021[,"Seq"]),]
# 
# #
# OTUdata_FU_AB_all<-OTUdata_FU_AB
# numberpresence_FU <- apply(OTUdata_FU_AB,2,function(X){sum(X!=0)})
# # plot(numberpresence_FU,pch=20,cex=.5, main="number of plots in which the ASV \n is present (only ASV with >100 reads) ")
# # abline(h=nrow(OTUdata_FU_AB)/10,col="blue")#10% cut 21
# # abline(h=nrow(OTUdata_FU_AB)/20,col="red") #5% cut 10
# # # abline(h=nrow(OTUdata_FU)-(nrow(OTUdata_FU)/10),col="blue")#10% cut
# # # abline(h=nrow(OTUdata_FU)-(nrow(OTUdata_FU)/20),col="red") #5% cut
# # plot(density(numberpresence_FU))
# # abline(v=nrow(OTUdata_FU_AB)/10,col="blue")#10% cut
# # abline(v=nrow(OTUdata_FU_AB)/20,col="red")#5% cut
# # #abline(v=nrow(OTUdata_FU)-(nrow(OTUdata_FU)/10),col="blue")#10% cut
# # #abline(v=nrow(OTUdata_FU_AB)-(nrow(OTUdata_FU_AB)/20),col="red") #5% cut
# 
# 
# 
# removed_toorare <- sum(numberpresence_FU<nrow(OTUdata_FU_AB)/20)
# removed_toogeneral <- sum(numberpresence_FU>nrow(OTUdata_FU_AB)-nrow(OTUdata_FU_AB)/20)
# removed_toorare_taxo<-OTUdata_FU_AB_taxo2[numberpresence_FU<nrow(OTUdata_FU_AB)/20,]
# removed_toogeneral_taxo<-OTUdata_FU_AB_taxo2[numberpresence_FU>nrow(OTUdata_FU_AB)-nrow(OTUdata_FU_AB)/20,]
# 
# # removed_toorare #13021
# # removed_toogeneral #27  #they are only removed in PA models
# # removed_toorare+removed_toogeneral #13048
# sum(removed_toorare_taxo$Kingdom=="Fungi",na.rm = TRUE)#13021
# sum(removed_toorare_taxo$Kingdom!="Fungi",na.rm = TRUE)#0 if removal above of non fungi # else 27869
# sum(is.na(removed_toorare_taxo$Kingdom!="Fungi"))
# 
# sum(removed_toogeneral_taxo$Kingdom=="Fungi",na.rm = TRUE)#27
# sum(removed_toogeneral_taxo$Kingdom!="Fungi",na.rm = TRUE)#0 if removal above of non fungi # else 7
# 
# # OTUdata_FU_PA <- OTUdata_FU_PA[,numberpresence_FU_PA>=nrow(OTUdata_FU_PA)/20]
# 
# #remove less prevalent ones for AB
# OTUdata_FU_AB <- OTUdata_FU_AB_all[,numberpresence_FU>=nrow(OTUdata_FU_AB_all)/20]
# OTUdata_FU_AB_taxo2<-FUtaxo2021[match(colnames(OTUdata_FU_AB),FUtaxo2021[,"Seq"]),]
# OTUdata_FU_PA <- apply(OTUdata_FU_AB,c(1,2),FUN=function(X){X>0}) 
# OTUdata_FU_PA2 <- OTUdata_FU_AB_all[,(numberpresence_FU>=nrow(OTUdata_FU_AB_all)/20)*(numberpresence_FU<nrow(OTUdata_FU_AB_all)-nrow(OTUdata_FU_AB_all)/20)]
# OTUdata_FU_PA2 <- apply(OTUdata_FU_PA2,c(1,2),FUN=function(X){X>0}) 
# 
# 
# 
# t1<-Sys.time()
# OTUdata_FU_AB_clr <- clr_unif(OTUdata_FU_AB,samples_are ="rows")
# Sys.time()-t1 #9min
# # dim(OTUdata_FU_AB)
# # ncol(OTUdata_FU_AB)# 17345
# # ncol(OTUdata_FU_PA)# 17345
# # ncol(OTUdata_FU_PA2)# 17318
# 
# 
# seqvec_ABdata <- colnames(OTUdata_FU_AB)
# names(seqvec_ABdata) <- paste0("OTU",1:ncol(OTUdata_FU_AB))
# all(OTUdata_FU_AB_taxo2$Seq==seqvec_ABdata)
# # saveRDS(seqvec_ABdata,file="seqvecFU.Rds")
# TotSeqSum_FU <- rowSums(OTUdata_FU_AB)
# 
# 
# 
# 
# #Protists
# OTUdata_PR_AB <- OTU_data_pro_179samples[,which(colnames(OTU_data_pro_179samples)%in%dataSoil$sampleNamePR)]
# 
# 
# OTUdata_PR_AB_taxo<-cbind(OTUdata_PR_AB ,PRtaxo2023[match(rownames(OTUdata_PR_AB),PRtaxo2023[,"Seq"]),])
# all(OTUdata_PR_AB_taxo$Seq==rownames(OTUdata_PR_AB_taxo))
# OTUdata_PR_AB <- t(OTUdata_PR_AB)
# 
# # nrow(OTUdata_PR_AB) #174
# # ncol(OTUdata_PR_AB) #11239
# # sum(OTUdata_PR_AB_taxo$Phylum!="Not_Protist",na.rm = TRUE) 2331 Protists
# # sum(OTUdata_PR_AB_taxo$Phylum=="Not_Protist",na.rm = TRUE) 8908 not Fungi (or unclassified)
# # sum(is.na(OTUdata_PR_AB_taxo$Phylum)) 0
# 
# # TotSeqSum_PR_AB <- rowSums(OTUdata_PR) #sequencing depth for "precleaned" data
# OTUdata_PR_AB <- OTUdata_PR_AB[rownames(ENVdata_PR), ] # #remove plots that dont have measures for all variables
# 
# # nrow(OTUdata_PR_AB) #166
# OTUdata_PR_AB <- OTUdata_PR_AB[,colSums(OTUdata_PR_AB)>99 ]# the selection of plots made some OTU to go under the threshold of 100 total read counts, remove them
# # ncol(OTUdata_PR_AB) #10860
# OTUdata_PR_AB_taxo2<-PRtaxo2023[match(colnames(OTUdata_PR_AB),PRtaxo2023[,"Seq"]),]
# all(OTUdata_PR_AB_taxo2$Seq==colnames(OTUdata_PR_AB))
# 
# # sum(OTUdata_PR_AB_taxo2$Phylum!="Not_Protist",na.rm = TRUE) 2270 Protists
# # sum(OTUdata_PR_AB_taxo2$Phylum=="Not_Protist",na.rm = TRUE) 8590 not Fungi (or unclassified)
# # sum(is.na(OTUdata_PR_AB_taxo$Phylum)) 0
# 
# # GCGGTAATTCCAGCTCCAATAGCGTATATTAAAGTTGTTGCAGTTAAAAAGCTCGTAGTCGAAGCTAGAGGCACCGGGGCGCGGGGGTCACTGACCGTCTGCGCCTGCGGGCCTGAACCGTATTCCGGTTTGCGCGTGTGCTTTGTTGTGTGCGTGCGTGCCGGAACGATTACCTTGAGAAAATTAGAGTGTTCAAAGCAGGCAGTTGCTCGAATACATTAGCATGGAATAATAAAAGAGGACTCGGGTTCTTTTTTGTTGGTTTATAGGACCGAGTAATGATTAATAGGAACGGTCGGGGGCATTGGTATTGCTGTGTCAGGAGTGAAATCTGGTGACCATGGTGGGACCAACAAGTGCAAAGGCATTTGCCAAGGACGTTTCCAT
# #Remove non protists
# # troph_euk<-read.csv("../../ASV_data/Troph_Prot.csv",sep=";")
# # troph_euk<-read.csv("Z:/projects-unil/SOMETALP/30_data/ASVtables_taxo/Tax_table_Protists.csv",sep=",")
# 
# # OTUdata_PR_AB <- OTUdata_PR_AB[,which(colnames(OTUdata_PR_AB)%in%troph_euk$X)]
# 
# 
# #easier datset for future (remove unassigned taxa directly instead of post-modelling): #ignored for publication 2023
# OTUdata_PR_AB <- OTUdata_PR_AB[,OTUdata_PR_AB_taxo2$Phylum!="Not_Protist"]
# OTUdata_PR_AB_taxo2<-PRtaxo2023[match(colnames(OTUdata_PR_AB),PRtaxo2023[,"Seq"]),]
# #
# # ncol(OTUdata_PR_AB ) #2270
# 
# OTUdata_PR_AB_all<-OTUdata_PR_AB
# numberpresence_PR <- apply(OTUdata_PR_AB,2,function(X){sum(X!=0)})
# # plot(numberpresence_PR,pch=20,cex=.5, main="number of plots in which the ASV \n is present (only ASV with >100 reads) ")
# # abline(h=nrow(OTUdata_PR_AB)/10,col="blue")#10% cut
# # abline(h=nrow(OTUdata_PR_AB)/20,col="red") #5% cut
# # # abline(h=nrow(OTUdata_PR)-(nrow(OTUdata_PR)/10),col="blue")#10% cut
# # # abline(h=nrow(OTUdata_PR)-(nrow(OTUdata_PR)/20),col="red") #5% cut
# # plot(density(numberpresence_PR))
# # abline(v=nrow(OTUdata_PR_AB)/10,col="blue")#10% cut
# # abline(v=nrow(OTUdata_PR_AB)/20,col="red")#5% cut
# # # abline(v=nrow(OTUdata_PR)-(nrow(OTUdata_PR)/10),col="blue")#10% cut
# # # abline(v=nrow(OTUdata_PR)-(nrow(OTUdata_PR)/20),col="red") #5% cut
# 
# removed_toorare <- sum(numberpresence_PR<nrow(OTUdata_PR_AB)/20)
# removed_toogeneral <- sum(numberpresence_PR>nrow(OTUdata_PR_AB)-nrow(OTUdata_PR_AB)/20)
# removed_toorare_taxo<-OTUdata_PR_AB_taxo2[numberpresence_PR<nrow(OTUdata_PR_AB)/20,]
# removed_toogeneral_taxo<-OTUdata_PR_AB_taxo2[numberpresence_PR>nrow(OTUdata_PR_AB)-nrow(OTUdata_PR_AB)/20,]
# # removed_toorare 108
# # removed_toogeneral 8  #they are only removed in PA models
# # removed_toorare+removed_toogeneral #116
# sum(removed_toorare_taxo$Phylum!="Not_Protist",na.rm = TRUE)#108
# sum(removed_toorare_taxo$Phylum=="Not_Protist",na.rm = TRUE)#0
# sum(is.na(removed_toorare_taxo$Phylum))
# 
# sum(removed_toogeneral_taxo$Phylum!="Not_Protist",na.rm = TRUE)#8
# sum(removed_toogeneral_taxo$Phylum=="Not_Protist",na.rm = TRUE)#0
# 
# 
# #remove less prevalent ones for AB
# OTUdata_PR_AB <- OTUdata_PR_AB_all[,numberpresence_PR>=nrow(OTUdata_PR_AB_all)/20]
# OTUdata_PR_AB_taxo2<-PRtaxo2023[match(colnames(OTUdata_PR_AB),PRtaxo2023[,"Seq"]),]
# OTUdata_PR_PA <- apply(OTUdata_PR_AB,c(1,2),FUN=function(X){X>0}) 
# OTUdata_PR_PA2 <- OTUdata_PR_AB_all[,(numberpresence_PR>=nrow(OTUdata_PR_AB_all)/20)*(numberpresence_PR<nrow(OTUdata_PR_AB_all)-nrow(OTUdata_PR_AB_all)/20)]
# OTUdata_PR_PA2 <- apply(OTUdata_PR_PA2,c(1,2),FUN=function(X){X>0}) 
# 
# t1<-Sys.time()
# OTUdata_PR_AB_clr <- clr_unif(OTUdata_PR_AB,samples_are ="rows")
# Sys.time()-t1 #35min
# 
# # ncol(OTUdata_PR_AB)# 2162
# # ncol(OTUdata_PR_PA)# 2162
# # ncol(OTUdata_PR_PA2)# 2154
# 
# # ncol(OTUdata_PR_AB) #2162
# # which(colnames(OTUdata_PR_AB)=="GCGGTAATTCCAGCTCCAATAGCGTATATTAAAGTTGTTGCAGTTAAAAAGCTCGTAGTCGAAGCTAGAGGCACCGGGGCGCGGGGGTCACTGACCGTCTGCGCCTGCGGGCCTGAACCGTATTCCGGTTTGCGCGTGTGCTTTGTTGTGTGCGTGCGTGCCGGAACGATTACCTTGAGAAAATTAGAGTGTTCAAAGCAGGCAGTTGCTCGAATACATTAGCATGGAATAATAAAAGAGGACTCGGGTTCTTTTTTGTTGGTTTATAGGACCGAGTAATGATTAATAGGAACGGTCGGGGGCATTGGTATTGCTGTGTCAGGAGTGAAATCTGGTGACCATGGTGGGACCAACAAGTGCAAAGGCATTTGCCAAGGACGTTTCCAT")
# #annoying  Champi = 3019
# 
# seqvec_ABdata <- colnames(OTUdata_PR_AB)
# names(seqvec_ABdata) <- paste0("OTU",1:ncol(OTUdata_PR_AB))
# all(OTUdata_PR_AB_taxo2$Seq==seqvec_ABdata)
# # save(seqvec_ABdata,file="seqvecPR_ABdata.Rda")
# 
# #cut at 10% : BA = 26, FU = 23, PR = 17 
# #cut at 5% : BA = 13 FU = 11 PR = 8
# #Where to cut here? 
# #10% removes most of fungi (most of them are present in a very few plots)
# #5% leaves some protists with only 9 plots to fit the models
# TotSeqSum_PR <- rowSums(OTUdata_PR_AB)

# datapoints<-runif(100,10,40)
# dporder<-datapoints[order(datapoints)]
# 
# data<-matrix(data=c(datapoints,))
# plot(dporder,dnorm(dporder,25,3)*7.5,type="l",lwd=5,ylim=c(0,1))
# abline(h=0)
# abline(h=1)
# points(dporder,rbinom(100,1,dnorm(dporder,25,3)*7.5),pch=20,col="blue",cex=2)

colnames(ENVdata_BA)
#save all 

dim(OTUdata_BA_PA);dim(OTUdata_BA_AB);dim(ENVdata_BA)
dim(OTUdata_BA_PA2);
saveRDS(OTUdata_BA_PA2,file="spat_exp_PA/BA/data/OTUdata_BA.Rds")
saveRDS(ENVdata_BA,file="spat_exp_PA/BA/data/ENVdata_BA.Rds")



# dim(OTUdata_FU_PA);dim(OTUdata_FU_AB);dim(ENVdata_FU)
# dim(OTUdata_FU_PA2);
# saveRDS(OTUdata_FU_PA,file="PA/FU/data/OTUdata_FU.Rds")
# saveRDS(OTUdata_FU_AB,file="AB/FU/data/OTUdata_FU.Rds")
# saveRDS(OTUdata_FU_AB_clr,file="AB/FU/data/OTUdata_FU_clr.Rds")
# saveRDS(OTUdata_FU_PA2,file="../../SESAM_micro/data/OTUdata_FU.Rds")
# saveRDS(TotSeqSum_FU,file="AB/FU/data/dataTotSeqSum_FU.Rds")
# saveRDS(ENVdata_FU,file="AB/FU/data/ENVdata_FU.Rds")
# saveRDS(ENVdata_FU,file="PA/FU/data/ENVdata_FU.Rds")


# dim(OTUdata_PR_PA);dim(OTUdata_PR_AB);dim(ENVdata_PR)
# dim(OTUdata_PR_PA2);
# saveRDS(OTUdata_PR_PA,file="PA/PR/data/OTUdata_PR.Rds")
# saveRDS(OTUdata_PR_AB,file="AB/PR/data/OTUdata_PR.Rds")
# saveRDS(OTUdata_PR_AB_clr,file="AB/PR/data/OTUdata_PR_clr.Rds")
# saveRDS(OTUdata_PR_PA2,file="../../SESAM_micro/data/OTUdata_PR.Rds")
# saveRDS(TotSeqSum_PR,file="AB/PR/data/dataTotSeqSum_PR.Rds")
# saveRDS(ENVdata_PR,file="AB/PR/data/ENVdata_PR.Rds")
# saveRDS(ENVdata_PR,file="PA/PR/data/ENVdata_PR.Rds")

write.csv(ENVdata_BA,file="spat_exp_PA/ENVdata_BA.csv")
# write.csv(ENVdata_FU,file="ENVdata_FU.csv")
# write.csv(ENVdata_PR,file="ENVdata_PR.csv")

#plot points
hillshade <- crop(readRDS("../../spatial_data/Valpar/ch_topo_alti3d2016_pixel_hillshade_mean.rds") |> rast(),ext(c(xmin=2552000,xmax=2587000,ymin=1114000,ymax=1157000)))
readRDS("../../spatial_data/Valpar/ch_topo_alti3d2016_pixel_hillshade_mean.rds") |> rast()
ENVstack<-ENVstack|> rast()
hillshade<-mask(hillshade,ENVstack$NDVI)

rownames( OTUdata_BA_AB)
edaph_data_BA2<-edaph_data_BA[edaph_data_BA$sampleNameBA%in%rownames(OTUdata_BA_AB),]
edaph_data_FU2<-edaph_data_FU[edaph_data_FU$sampleNameFU%in%rownames(OTUdata_FU_AB),]
edaph_data_PR2<-edaph_data_PR[edaph_data_PR$sampleNamePR%in%rownames(OTUdata_PR_AB),]

#BA points that are in FU and in PR
# BA_FU_PR<-edaph_data_BA2[edaph_data_BA2$x%in%edaph_data_FU2$x&edaph_data_BA2$y%in%edaph_data_FU2$y&edaph_data_BA2$x%in%edaph_data_PR2$x&edaph_data_BA2$y%in%edaph_data_PR2$y,c("x","y")]
# #Ba points that are in FU but not in PR
# BA_FU<-edaph_data_BA2[edaph_data_BA2$x%in%edaph_data_FU2$x&edaph_data_BA2$y%in%edaph_data_FU2$y&(!(edaph_data_BA2$x%in%edaph_data_PR2$x))&(!(edaph_data_BA2$y%in%edaph_data_PR2$y)),c("x","y")]
# #BA points that are in Pr but not in FU
# BA_PR<-edaph_data_BA2[edaph_data_BA2$x%in%edaph_data_PR2$x&edaph_data_BA2$y%in%edaph_data_PR2$y&(!(edaph_data_BA2$x%in%edaph_data_FU2$x))&(!(edaph_data_BA2$y%in%edaph_data_FU2$y)),c("x","y")]
# #Ba points that are neither in FU or PR
# BA_only<-edaph_data_BA2[(!(edaph_data_BA2$x%in%edaph_data_PR2$x))&(!(edaph_data_BA2$y%in%edaph_data_PR2$y))&(!(edaph_data_BA2$x%in%edaph_data_FU2$x))&(!(edaph_data_BA2$y%in%edaph_data_FU2$y)),c("x","y")]

library(viridis)
viridis(4)
plot(hillshade,col=gray.colors(1000),legend=FALSE, axes=FALSE)
points(edaph_data_BA2[,c("x","y")],pch=21,lwd=1,col="black",bg="#FDE725FF",cex=1)
# points(BA_FU_PR,pch=21,lwd=1,col="black",bg="#FDE725FF",cex=1)
# points(BA_FU,pch=22,lwd=1,col="black",bg="#35B779FF",cex=1)
# points(BA_PR,pch=23,col="black",bg="#31688EFF",cex=1)
# points(BA_only,pch=24,col="black",bg="#440154FF",cex=1)

# points(edaph_data_BA[,c("x","y")],pch=16,col="#aec800",cex=1.2)
# points(edaph_data_FU[,c("x","y")],pch=17,col=alpha("#6B4C62",0.8))
# points(edaph_data_PR[,c("x","y")],pch=18,col=alpha("#457EB0",0.7))
# legend(2555000,1130000,c("BAFP","BAF", "BAP","BA"),pch=c(21,22,23,24), col=rep("black",4),pt.bg=c("#FDE725FF","#35B779FF","#31688EFF","#440154FF"),bty="n")


# png(file=paste0("figures/PAAB_selection/Datasets_sampling.png"),res=300,width=1961,height=1500)
# plot(hillshade,col=gray.colors(1000),legend=FALSE, axes=FALSE)
# points(BA_FU_PR,pch=21,lwd=1,col="black",bg="#FDE725FF",cex=.8)
# points(BA_FU,pch=22,lwd=1,col="black",bg="#35B779FF",cex=.8)
# points(BA_PR,pch=23,col="black",bg="#31688EFF",cex=.8)
# points(BA_only,pch=24,col="black",bg="#440154FF",cex=.8)
# legend(2552000,1130000,c("BAFP","BAF", "BAP","BA"),cex=.8,pch=c(21,22,23,24), col=rep("black",4),pt.bg=c("#FDE725FF","#35B779FF","#31688EFF","#440154FF"),bty="n")
# 
# dev.off()
