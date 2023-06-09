library(DT)
library(data.table)
library(dplyr)
library(readxl)
library(tidyr)
library(RColorBrewer)
library(circlize)
library(Seurat)
library(monocle)
library(geneName)
library(RMySQL)
source("functions.R")

#rmdfiles <- rmarkdown::render("about.Rmd")
#sapply(rmdfiles, knitr::knit, quiet = T)
mouse_orthology<-data.table::fread("./data/HGNC_AllianceHomology.rpt")
colnames(mouse_orthology)[1:16]<-colnames(mouse_orthology)[2:17]
mouse_orthology<-mouse_orthology[,-17]
colnames(mouse_orthology)[2]<-"Gene_name"

mouse_synonyms<-data.table::fread("./data/MRK_List2.rpt")
colnames(mouse_synonyms)[c(7,12)]<-c("Gene_name","Gene_synonyms")
mouse_synonyms<-tidyr::separate_rows(mouse_synonyms, Gene_synonyms, sep = "\\|") %>% select(7,12) %>% unique()
mouse_synonyms<-mouse_synonyms[mouse_synonyms$Gene_synonyms != "",]
mouse_orthology<-left_join(mouse_orthology, mouse_synonyms, by = "Gene_name")


# rbt<-data.frame(Gene_name = rownames(pandora_rabbit))
# rbt$Gene_name<-str_split(rbt$Gene_name, "\\.", simplify = TRUE)[,1]
# 
# rbt$Gene_name2<-mouse_orthology$`HGNC ID`[match(rbt$Gene_name, mouse_orthology$Gene_name)]
# rbt$Gene_name3<-mouse_orthology$`HGNC ID`[match(rbt$Gene_name, mouse_orthology$Gene_synonyms)]
# rbt$Gene_name2<-with(rbt, ifelse(is.na(Gene_name2), Gene_name3, Gene_name2))
# rbt$Gene_name2<-with(rbt, ifelse(is.na(Gene_name2), Gene_name, Gene_name2))


hgnc_names<-fread("./data/hgnc_names.txt") %>% filter(Status == "Approved") %>% dplyr::select(1,3)
mouse_orthology2<-tidyr::separate_rows(mouse_orthology, `HGNC ID`, sep = "\\|")
mouse_orthology2<-mouse_orthology2[,c(1,2,5,10,16,17)]
mouse_orthology2<-left_join(mouse_orthology2, hgnc_names, by = "HGNC ID")


# pandora_rabbit_full<-readRDS("./data/sc_data/12_Rabbit_Lung.rds")
# data_to_write_out <- as.data.frame(as.matrix(pandora_rabbit_full@assays$RNA@data))
# fwrite(x = data_to_write_out, row.names = TRUE, file = "rabbit.csv")

lung<-readRDS("./data/lung_reduced.RDS")
#lung<-readRDS(url("https://drive.google.com/uc?export=download&id=1Q9WKkQml3woMnvvHPj_whj9O37a5cMjF","rb"))
lung_idents<-Idents(object = lung)
cell_types_lung<-levels(lung_idents)
lung_markers<-readRDS("./data/markers.RDS")
lung_markers<-Map(cbind, lung_markers, "gene" = lapply(lung_markers, rownames))
lung_markers<-lapply(lung_markers, function(x) relocate(x, gene, .before = p_val))

reyfman<-readRDS("./data/reyfmans_seurat_reduced.RDS")
reyfman_markers<-readRDS("./data/markers_reyfman.RDS") %>%
  relocate(gene, .before = p_val)

habermann<-readRDS("./data/banovich_reduced.RDS")
habermann_markers<-fread("./data/banovich_markers.txt") %>%
  relocate(gene, .before = p_val)

pandora_tiger<-readRDS("./data/sc_data/tiger_reduced.rds")
pandora_pangolin<-readRDS("./data/sc_data/pangolin_reduced.rds")
pandora_deer<-readRDS("./data/sc_data/deer_reduced.rds")
pandora_goat<-readRDS("./data/sc_data/goat_reduced.rds")
pandora_rabbit<-readRDS("./data/sc_data/rabbit_reduced.rds")
pandora_cat<-readRDS("./data/sc_data/cat_reduced.rds")
pandora_dog<-readRDS("./data/sc_data/dog_reduced.rds")
pandora_hamster<-readRDS("./data/sc_data/hamster_reduced.rds")
pandora_lizard<-readRDS("./data/sc_data/lizard_reduced.rds")
pandora_duck<-readRDS("./data/sc_data/duck_reduced.rds")
pandora_pigeon<-readRDS("./data/sc_data/pigeon_reduced.rds")
pandora_bat<-readRDS("./data/sc_data/bat_reduced.rds")


cele<-readRDS("./data/cele_seurat.RDS")

pandora.list<-data.frame(paper = c("Tiger Lung","Pangolin Lung","Deer Lung","Goat Lung","Rabbit Lung","Cat Lung",
                                   "Dog Lung","Hamster Lung","Lizard Lung","Duck Lung","Pigeon Lung","Bat Lung"),
                         data = c("pandora_tiger","pandora_pangolin","pandora_deer","pandora_goat","pandora_rabbit",
                                  "pandora_cat","pandora_dog","pandora_hamster","pandora_lizard","pandora_duck",
                                  "pandora_pigeon","pandora_bat"))

sc.paper.list<-data.frame(paper = c("Carraro et al(2021) - Lung (human)",
                                    "Reyfman et al(2018) - Lung (human)",
                                    "Habermann et al(2020) - Lung (human)",
                                    "Chen et al(2021) - Lung(mammals, reptiles, birds)",
                                    "Cao et al(2017) - C. elegans"),
                          data = c("lung","reyfman","habermann","pandora.list","cele"))


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
ciliaryGenes1$gold<-"YES"
#final_score_table<-fread("./data/new_scores.txt")


db<-load_data_mysql()
final_score_table<-tbl(db, "new_scores") %>%
  collect()
homsap<-tbl(db, "gene_info_edited") %>%
  collect()
orthology<-tbl(db, "orthology") %>%
  collect()
orthology<-hgncConverter(orthology, "Gene2Symbol")
colnames(orthology)[1]<-"Gene_name"
orthology<-semi_join(orthology, homsap, by = "Gene_name")
colnames(orthology)[1]<-"Gene2Symbol"
dbDisconnect(db)

ciliogenics<-final_score_table %>%
  filter(Mean_score >= 0.4)

# homsap<-fread("./data/Homo_sapiens.gene_info")
# homsap2<-homsap %>%
#   separate_rows(Synonyms, sep = "\\|")



#ens<-fread("./data/ens.txt")

#orthology<-fread("./data/orthology.txt")

# meta.data<-orthology
# colnames(meta.data)[3]<-"Gene.Name"
# meta.data<-left_join(meta.data, ciliaryGenes1, by = "Gene.Name")



# biogrid<-fread("./data/biogrid.csv")
# intact<-fread("./data/intact.csv")
# wbP<-fread("./data/wormbaseP1.csv")

# load_data_mysql <- function(query) {
#   db <- dbConnect(MySQL(), dbname = "ciliogenics",
#                   host = Sys.getenv("host"),
#                   port = 3306,
#                   user = Sys.getenv("user"),
#                   password = Sys.getenv("password"))
#   #query <- sprintf("SELECT * FROM %s", TABLE_NAME)
#   #query <- sprintf("SELECT * FROM %s WHERE `Interactor A` = '%s'", TABLE_NAME, gname)
#   #query <- paste0("SELECT * FROM ", TABLE_NAME, " WHERE `Interactor A` = ", gname)
#   data <- dbGetQuery(db, query)
#   dbDisconnect(db)
#   data
# }


#prot.int<-load_data_mysql("protein_interactions")

# Prot. int.
# con <- dbConnect(RMySQL::MySQL(), host = 'localhost', user = 'user', password = 'pswd', dbname = 'ciliogenics', port = 3306)
# prot.int<-tbl(con, "protein_interactions") %>%
#   collect()
#
# # Genetic int.
# gen.int<-tbl(con, "genetic_interactions") %>%
#   collect()
#
#



#publications<-fread("./data/publications.csv")
#publications<-fread("./data/publications_17.06.21.txt")
# publ<-read_xlsx("./data/Publications.xlsx")
nscores2<-fread("./data/nscores21.csv")
nscores2<-hgncConverter(nscores2, "Gene_name")
aa<-fread("./data/aa.csv")
#aa<-hgncConverter(aa, "Gene_name")
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

# final_seq_table<-final_score_table
#
# for (i in 2:11){
#
#   final_seq_table<-final_seq_table[order(final_seq_table[[i]], decreasing = TRUE),]
#   final_seq_table[[i]]<-c(1:length(final_seq_table[[i]]))
# }

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



# omim<-fread("./data/mim2gene.txt", skip = 4) %>%
#   filter(`MIM Entry Type (see FAQ 1.3 at https://omim.org/help/faq)` == "gene") %>%
#   filter(`Approved Gene Symbol (HGNC)` != "")
# colnames(omim)[c(1,4)]<-c("omim_id","Gene_name")


lst<-readRDS("./data/lst.RDS")
lst[sapply(lst, is.null)] <- NULL

# pub_mat<-readRDS("./data/pub_mat.RDS")
# pub_mat2<-pub_mat
# pub_mat2$all<-rowSums(pub_mat2[,-1])
# pub_mat2<-pub_mat2[order(-pub_mat2$all),]
# pub_mat3<-pub_mat2[1:11,]
# pub_mat3<-pub_mat3[,-56]
# colnames(pub_mat2)[1]<-"Gene_name"
# colnames(pub_mat)[1]<-"Gene_name"
#
# temp_pub<-anti_join(final_score_table, pub_mat2, by = "Gene_name")
# pub_mat4<-data.frame(Gene_name = temp_pub$Gene_name)
# pub_mat4[colnames(pub_mat)[-1]]<-0
# pub_mat<-rbind(pub_mat, pub_mat4)
#
# temp2<-anti_join(pub_mat, final_score_table, by = "Gene_name")
# pub_mat<-anti_join(pub_mat, temp2, by = "Gene_name")
# rownames(pub_mat)<-pub_mat$Gene_name

pubgenelist<-c("IFT172", "IFT57", "IFT88", "PACRG", "TRAF3IP1", "EFHC1", "IFT80", "DNAI1", "DYNC2LI1", "NME7", "TTC26")

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

#pub_genes<-pub_mat[which(apply(pub_mat[,-1],1,sum) != 0),]

cookie_box <- div(class="alert alert-info", style = "margin-bottom: 0; bottom: 70px; position: fixed; width: 80%",
                  "This website places cookies on your device to help us improve our service
      to you", HTML('<a href="#" class="close" data-dismiss="alert" aria-label="close">X</a>'))


# all_scores<-fread("./data/new_scores_all.txt")
# df_n1<-all_scores[,c(1,3,7,11,15,19,23,27)]
# colnames(df_n1)<-colnames(final_score_table)[1:8]
# #df_n <- as.data.table(lapply(final_score_table[,2:8], min_max_norm))
# df_n1<-round(df_n1[,2:8], digits = 3)
# df_n1<-df_n1[,c(2,1,4,3,5:7)]
#df_n1<-df_n1[,1:7]

# Pub chart
# xc<-publications %>% dplyr::count(Publication)
# xc$type<-"total"
# xc$cil<-"all"
#
# xc1<-publications
# xc1$cil<-"unknown"
# xc1$cil[which(xc1$Gene_name %in% ciliaryGenes1$Gene.Name)]<-"ciliary"
# xc2<-xc1 %>% count(Publication, cil)
# xc2<-xc2[xc2$cil == "ciliary",]
# xc2$type<-"ciliary"
# xc3<-rbind(xc,xc2)
# colnames(xc3)[2]<-"Number of genes"
#
#
# xc4<-publications %>% dplyr::add_count(Gene_name)
# xc4$`is cilliary`<-"No"
# xc4$`is cilliary`[xc4$Gene_name %in% ciliaryGenes1$Gene.Name]<-"Yes"
# xc4$`is unique`<-ifelse(xc4$n == 1, "Yes", "No")

motiflist<-c("",unique(motifs$`Motif ID`))
names(motiflist)<-c("", unique(paste(motifs$`Motif ID`, motifs$Motif, sep = "-")))

sources<-read_xlsx("./data/source.xlsx", col_names = FALSE)
colnames(sources)<-c("Source","Link","Link1")
sources$Link <- paste0("<a href='",sources$Link1,"' target='_blank'>",sources$Link,"</a>")

downloads<-read_xlsx("./data/downloads.xlsx")
downloads$Link<-paste0("<a href='",downloads$Link1,"' target='_blank'>",downloads$Link,"</a>")
colnames(downloads)[2]<-"Download"

downloads2<-read_xlsx("./data/downloads2.xlsx")
downloads2$Download<-paste0("<a href='",downloads2$Download,"' target='_blank'>",downloads2$Download,"</a>")

db<-load_data_mysql()
protein_atlas<-tbl(db, "protein_atlas") %>%
  collect()
dbDisconnect(db)


