## ----include=FALSE------------------------------------------------------------
library(rtracklayer)
library(annotatr)
library(data.table)
library(RnBeads)
library(ChIPseeker)
library(clusterProfiler)
library(ggplot2)


## -----------------------------------------------------------------------------
#Use the bed files from the tracks and tables folder
# Check for the gene LIG4

## -----------------------------------------------------------------------------


#file <- fread(file="data/diffMethTable_site_cmp1.csv", sep = ",")


  #bg <- copy(file)
  #bg <- bg[,.(Chromosome, Start, End=Start+1, cgid)]
  #fwrite(bg, row.names=F, file = "rnb_run_pilot/differential_methylation_data/background.txt", quote = F, sep="\t",  col.names = F)

#hyper <- file[diffmeth.p.val<0.0001 & mean.diff>0.3,]
#great_hyper <- hyper[,.(Chromosome, Start, End=Start+1, cgid)]

#hypo <- file[diffmeth.p.val<10e-14 & mean.diff< -0.7,]
#great_hypo <- hypo[,.(Chromosome, Start, End=Start+1, cgid)]

#fwrite(file, row.names=F, file = paste0(folder, "diffMethTable_significant01_cmp_", i, ".txt"), quote = F, sep="\t")

#fwrite(great_hyper, row.names=F, file = "rnb_run_pilot/differential_methylation_data/great_significant_hyper.txt", quote = F, sep="\t", col.names = F)
#fwrite(great_hypo, row.names=F, file = "rnb_run_pilot/differential_methylation_data/great_significant_hypo.txt", quote = F, sep="\t", col.names = F)



data <- list()
data[["hypo"]]  <- import.bed("../inst/extdata/great_significant_hypo.txt")
data[["hyper"]]<- import.bed("../inst/extdata/great_significant_hyper.txt")


annots = c('hg19_Hepg2-chromatin')
annots_gr = build_annotations(genome = 'hg19', annotations = annots)


for (dataset in data){
genome(dataset) <- rep("hg19", length(genome(dataset)))
result <- annotate_regions(dataset, annotations=annots_gr, minoverlap = 1L, ignore.strand = TRUE, quiet = FALSE)
rnd_annots = annotate_regions(regions = randomize_regions(dataset),annotations = annots_gr,ignore.strand = TRUE)
p_annots = plot_annotation(annotated_regions = result, annotated_random = rnd_annots)
print(p_annots)
}


background <-  import.bed("../inst/extdata/background.txt")
genome(background) <- rep("hg19", length(genome(background)))


for (dataset in names(data)){
genome(data[[dataset]]) <- rep("hg19", length(genome(data[[dataset]])))
background2 <- background[sample(1:length(background), length(data[[dataset]])),]
result <- annotate_regions(data[[dataset]], annotations=annots_gr, minoverlap = 1L, ignore.strand = TRUE, quiet = FALSE)
rnd_annots = annotate_regions(regions = background2,annotations = annots_gr,ignore.strand = TRUE)
p_annots = plot_annotation(annotated_regions = result, annotated_random = rnd_annots)
print(p_annots+ggtitle(dataset))
}



## -----------------------------------------------------------------------------

builtin_annotations()


## -----------------------------------------------------------------------------


library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

for (dataset in names(data)){
  genes <- seq2gene(data[[dataset]], tssRegion = c(-3000, 3000), flankDistance = 3000, TxDb=txdb)
  kkm <- enrichKEGG(gene         = genes,
                 organism     = 'human',
                 pvalueCutoff = 0.05)
  

  print(dotplot(kkm))
  
  engo <- enrichGO(gene          = genes,
                OrgDb         = org.Hs.eg.db,
                ont           = "BP",
                pAdjustMethod = "BH",
                pvalueCutoff  = 0.01,
                qvalueCutoff  = 0.05,
        readable      = TRUE)
  print(dotplot(engo))
  
  }


