library(DT)
library(data.table)
library(dplyr)
library(readxl)
library(tidyr)
library(RColorBrewer)

ciliaryGenes1<-fread("./data/ciliaryGenes1.txt")
final_score_table<-fread("./data/ciliogenics_ordered_list.csv", sep = ",")
ciliogenics<-final_score_table %>%
  filter(Weighted_total_scores >= 0.5)
#final_score_table1<-final_score_table
# final_score_table1$goldstandard<-"NO"
# final_score_table1$goldstandard[which(final_score_table1$goldstandard %in% ciliaryGenes1[[1]])]<-"YES"
ciliaryGenes1$gold<-"YES"

homsap<-fread("./data/Homo_sapiens.gene_info")
homsap2<-homsap %>% 
  separate_rows(Synonyms, sep = "\\|")

ens<-fread("./data/ens.txt")

biogrid<-fread("./data/biogrid.csv")
intact<-fread("./data/intact.csv")
wbP<-fread("./data/wormbaseP1.csv")

publications<-fread("./data/publications.csv")
publ<-read_xlsx("./data/Publications.xlsx")
nscores2<-fread("./data/nscores21.csv", )
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

# Heatmap

cgenes<-fread("./data/ciliarygenes.tsv")
cgenes[which(cgenes$C_elegans == "K08D12.2")]<-NA
cgenes<-na.omit(cgenes)
df<-fread("./data/C_elegans_single_cell.csv")
df1<-df[which(df$symbol %in% cgenes$C_elegans),]
a<-cgenes[-which(cgenes$C_elegans %in% df1$symbol),]
df2<-df[match(cgenes$C_elegans, df$symbol),] %>%
  na.omit() %>%
  relocate(Ciliated_sensory_neurons, .after = Intestine) %>%
  relocate(Oxygen_sensory_neurons, .after = Intestine)

df3<-df2[,-1]
df3<-df3[,-1]
mm<-as.matrix(df3)
rownames(mm)<-df2$symbol
colnames(mm)[19]<-"flp-1_interneurons"

mypalette<-brewer.pal(7,"Blues")
annot<-data.frame(genes = cgenes$C_elegans)
annot$`Gene type` <- rep(c("Known ciliary genes", "Putative ciliary genes"), c(51, 26))
rownames(annot)<-annot$genes
annot1<-dplyr::select(annot,`Gene type`)
annotcol<-data.frame("Cell types" = factor(rep(c("Non-ciliated cells", "Ciliated cells"), c(25, 2))))
colnames(annotcol)<-"Cell types"
rownames(annotcol)<-colnames(mm)

ann_color<-list(
  `Cell types` = c(`Non-ciliated cells` = "#808080", `Ciliated cells` = "#FF007F"),
  `Gene type` = c(`Known ciliary genes` = "#FF8C00", `Putative ciliary genes` = "#00BFFF")
)


orange_pal <- function(x){
  if (!is.na(x)){
    rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)
  } else {
    "#e9e9e9" #grey
  }
}

stylefunc <- function(value, index, name) {
  normalized <- (value - min(df4[name], na.rm = T)) /
    (max(df4[name], na.rm = T) - min(df4[name], na.rm = T))
  color <- orange_pal(normalized)
  list(background = color)
}

coldefs <- list(
  reactable::colDef(style = stylefunc, minWidth = 100)
)


df4<-data.frame(df2, stringsAsFactors = FALSE)
rownames(df4)<-df1$symbol
#df4<-df4[,c(-1,-2)]
numcols <- df4 %>% dplyr::select(where(is.numeric)) %>% colnames()
# replicate list to required length
coldefs <- rep(coldefs,length(numcols))
# name elements of list according to cols
names(coldefs) <- numcols



omim<-fread("./data/mim2gene.txt", skip = 4) %>%
  filter(`MIM Entry Type (see FAQ 1.3 at https://omim.org/help/faq)` == "gene") %>%
  filter(`Approved Gene Symbol (HGNC)` != "")
colnames(omim)[c(1,4)]<-c("omim_id","Gene_name")
