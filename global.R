library(DT)
library(data.table)
library(dplyr)
library(readxl)
library(tidyr)
library(RColorBrewer)
library(circlize)
library(Seurat)
library(monocle)
source("functions.R")

#rmdfiles <- rmarkdown::render("about.Rmd")
#sapply(rmdfiles, knitr::knit, quiet = T)

lung<-readRDS("./data/lung_reduced.RDS")
#lung<-readRDS(url("https://drive.google.com/uc?export=download&id=1Q9WKkQml3woMnvvHPj_whj9O37a5cMjF","rb"))
lung_idents<-Idents(object = lung)
cell_types_lung<-levels(lung_idents)
lung_markers<-readRDS("./data/markers.RDS")

reyfman<-readRDS("./data/reyfmans_seurat_reduced.RDS")
reyfman_markers<-readRDS("./data/markers_reyfman.RDS")

habermann<-readRDS("./data/banovich_reduced.RDS")
habermann_markers<-fread("./data/banovich_markers.txt")

cele<-readRDS("./data/cele_seurat.RDS")
sc.paper.list<-data.frame(paper = c("Carraro et al(2021) - Lung (human)", 
                                    "Reyfman et al(2018) - Lung (human)", 
                                    "Habermann et al(2020) - Lung (human)",
                                    "Cao et al(2017) - C. elegans"),
                          data = c("lung","reyfman","habermann","cele"))
lung_names<-rownames(lung)
reyfman_names<-rownames(reyfman)
habermann_names<-rownames(habermann)
cele_names<-rownames(cele)

load("./data/Cao_et_al_2017_vignette.RData")
cele_data<-pData(cds)
cele_data$cell.type<-as.factor(cele_data$cell.type)

label.df <- data.frame(cell.type=levels(cele_data$cell.type),label=levels(cele_data$cell.type))
label.df_2 <- cele_data %>%
  group_by(cell.type) %>%
  summarize(tsne_1 = mean(tsne_1), tsne_2 = mean(tsne_2)) %>%
  left_join(label.df)

cele_genes<-fData(cds)

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

orthology<-fread("./data/orthology.txt")

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
anot<-data.frame(Class = species[,2], Organisms = rep(c("Ciliary", "Nonciliary"), c(44,28)))
row.names(anot)<-species$X1
colnames(anot)[1]<-"Class"

my_colour = list(Organisms = c(Ciliary = "firebrick3", Nonciliary = "dodgerblue3"), 
                 Class = c(Animals = "firebrick3", Fungi = "dodgerblue3", Protists = "darkgrey", Plants = "chartreuse", Other = "ghostwhite", Bacteria = "gray0"))
gene_synonyms2<-fread("./data/gene_synonyms2.csv")

final_seq_table<-final_score_table

for (i in 2:11){
  
  final_seq_table<-final_seq_table[order(final_seq_table[[i]], decreasing = TRUE),]
  final_seq_table[[i]]<-c(1:length(final_seq_table[[i]]))
}

# Single cell clusters

aa1<-fread("./data/aa1_whom3.txt")
celegans_sc<-fread("./data/celegans_sc_binary_whom.txt")



cells<-colnames(celegans_sc)[c(2:28)]
anot_sc<-data.frame(`Cell types` = rep(c("Nonciliary", "Ciliary"), c(25,2)))
row.names(anot_sc)<-cells
colnames(anot_sc)<-"Cell type"

my_colour_sc = list(`Cell types` = c(Ciliary = "firebrick3", Nonciliary = "dodgerblue3"))




# Motifs

motifs<-fread("./data/motif_list.txt")

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


lst<-readRDS("./data/lst.RDS")
lst[sapply(lst, is.null)] <- NULL

pub_mat<-readRDS("./data/pub_mat.RDS")
pub_mat2<-pub_mat
pub_mat2$all<-rowSums(pub_mat2[,-1])
pub_mat2<-pub_mat2[order(-pub_mat2$all),]
pub_mat3<-pub_mat2[1:11,]
pub_mat3<-pub_mat3[,-56]
colnames(pub_mat2)[1]<-"Gene_name"
colnames(pub_mat)[1]<-"Gene_name"

temp_pub<-anti_join(final_seq_table, pub_mat2, by = "Gene_name")
pub_mat4<-data.frame(Gene_name = temp_pub$Gene_name)
pub_mat4[colnames(pub_mat)[-1]]<-0
pub_mat<-rbind(pub_mat, pub_mat4)

temp2<-anti_join(pub_mat, final_seq_table, by = "Gene_name")
pub_mat<-anti_join(pub_mat, temp2, by = "Gene_name")
rownames(pub_mat)<-pub_mat$Gene_name

col_fun = colorRamp2(c(1, 0), c("blue4", "antiquewhite"))

# pub_mat4<-melt(pub_mat3)
# colnames(pub_mat4)<-c("Gene name", "Publication", "Value")
# 
# p<-ggplot(pub_mat4, aes(Publication, `Gene name`)) +
#   geom_tile(aes(fill = Value), colour = "black") +
#   scale_fill_gradient(low = "white",high = "steelblue")
# 
# pt<-p + theme_grey() + 
#   labs(x = "", y = "") +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   theme(legend.position = "none")
#                                              
# 
# ggplotly(pt)
# 
# library(heatmaply)
# heatmaply(as.matrix(rbind(pub_mat[pub_mat$V1 == "ARL13B",-1], pub_mat3[,-1])), grid_color = "black", seriate = "none",
#           column_text_angle = 90, Rowv = FALSE, Colv = FALSE, show_dendrogram = c(FALSE, FALSE), hide_colorbar = TRUE, 
#           colors = c("azure","blue4"))

pub_genes<-pub_mat[which(apply(pub_mat[,-1],1,sum) != 0),]

cookie_box <- div(class="alert alert-info", style = "margin-bottom: 0; bottom: 70px; position: fixed; width: 80%",
                  "This website places cookies on your device to help us improve our service 
      to you", HTML('<a href="#" class="close" data-dismiss="alert" aria-label="close">X</a>'))


df_n <- as.data.table(lapply(final_score_table[,2:8], min_max_norm))
df_n1<-round(df_n, digits = 3)


# Pub chart
xc<-publications %>% dplyr::count(Publication)
xc$type<-"total"
xc$cil<-"all"

xc1<-publications
xc1$cil<-"unknown"
xc1$cil[which(xc1$Gene_name %in% ciliaryGenes1$Gene.Name)]<-"ciliary"
xc2<-xc1 %>% count(Publication, cil)
xc2<-xc2[xc2$cil == "ciliary",]
xc2$type<-"ciliary"
xc3<-rbind(xc,xc2)
colnames(xc3)[2]<-"Number of genes"


xc4<-publications %>% dplyr::add_count(Gene_name)
xc4$`is cilliary`<-"No"
xc4$`is cilliary`[xc4$Gene_name %in% ciliaryGenes1$Gene.Name]<-"Yes"
xc4$`is unique`<-ifelse(xc4$n == 1, "Yes", "No")

motiflist<-c("",unique(motifs$`Motif ID`))
names(motiflist)<-c("", unique(paste(motifs$`Motif ID`, motifs$Motif, sep = "-")))

