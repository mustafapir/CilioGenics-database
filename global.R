
library(DT)
library(data.table)
library(dplyr)
library(readxl)

ciliaryGenes1<-fread("./data/ciliaryGenes1.txt")
final_score_table<-fread("./data/ciliogenics_ordered_list.csv", sep = ",")
ciliogenics<-final_score_table %>%
  filter(Weighted_total_scores >= 0.5)
#final_score_table1<-final_score_table
# final_score_table1$goldstandard<-"NO"
# final_score_table1$goldstandard[which(final_score_table1$goldstandard %in% ciliaryGenes1[[1]])]<-"YES"
ciliaryGenes1$gold<-"YES"

homsap<-fread("./data/Homo_sapiens.gene_info")
ens<-fread("./data/ens.txt")

biogrid<-fread("./data/biogrid.csv")
intact<-fread("./data/intact.csv")
wbP<-fread("./data/wormbaseP1.csv")

publications<-fread("./data/publications.csv")
publ<-read_xlsx("./data/Publications.xlsx")
nscores2<-fread("./data/nscores2.csv", )
aa<-fread("./data/aa.csv")
row.names(nscores2)<-nscores2$gene_name

species<-fread("./data/species_app.txt")
species<-species[,-1]

species<-species[which(species$X1 %in% colnames(nscores2[,2:73])),]
anot<-data.frame(Class = species[,2], organisms = rep(c("Ciliary", "Nonciliary"), c(44,28)))
row.names(anot)<-species$X1
colnames(anot)[1]<-"Class"

my_colour = list(organisms = c(Ciliary = "firebrick3", Nonciliary = "dodgerblue3"), 
                 Class = c(Animals = "firebrick3", Fungi = "dodgerblue3", Protists = "darkgrey", Plants = "chartreuse", Other = "ghostwhite", Bacteria = "gray0"))
gene_synonyms2<-fread("./data/gene_synonyms2.csv")

final_seq_table<-final_score_table

for (i in 2:11){
  
  final_seq_table<-final_seq_table[order(final_seq_table[[i]], decreasing = TRUE),]
  final_seq_table[[i]]<-c(1:length(final_seq_table[[i]]))
}

