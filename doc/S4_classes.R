## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
setClass("Person", representation(name = "character", age = "numeric"))
setClass("Employee", representation(boss = "Person"), contains = "Person")

person1 <- new("Person", name="Peter", age=34)
boss1 <- new("Employee", boss=person1, name="Lisa", age=36)

## -----------------------------------------------------------------------------

person1
boss1


## -----------------------------------------------------------------------------
library(SummarizedExperiment)
data("airway_small",  package="BasicR")

se <- airway_small


## -----------------------------------------------------------------------------
colData(se)
rowData(se)
#assay(se)
rowRanges(se)
#assays(se)[[1]]
head(assays(se)$counts)

head(metadata(airway_small))


## -----------------------------------------------------------------------------
se@rowRanges
se@metadata

###don't do this:
se@rowRanges <- se@rowRanges[1:3000]
se
se@rowRanges



## -----------------------------------------------------------------------------
# subsetting is very easy, just like with a data.frame

se <- se[1:2000,]

se <- se[,1:3]

se


## -----------------------------------------------------------------------------

rowbinded <- rbind(se, se)
rowbinded
colbinded <- cbind(se, se)
colbinded


## -----------------------------------------------------------------------------
roi <- GRanges(seqnames="X", ranges=99800000:99850000)
se_1 <- subsetByOverlaps(se, roi)




## -----------------------------------------------------------------------------
library(GenomicRanges)

gr1 <- GRanges(
    seqnames = "chr2",
    ranges = IRanges(103, 106),
    strand = "+",
    score = 5L, GC = 0.45)
gr2 <- GRanges(
    seqnames = c("chr1", "chr3"),
    ranges = IRanges(c(107, 113), width = 3),
    strand = c("+", "-"),
    score = 3:4, GC = c(0.3, 0.5))
gr3 <- GRanges("chr2:102-107")

gr4 <- makeGRangesFromDataFrame(data.frame(chr=c("chr1", "chr2"), 
                                           start=c(104, 104), end=c(108, 105),
                                           name=c("gene1", "gene2")), keep.extra.columns = T, ignore.strand = T)
grl <- GRangesList("txA" = gr1, "txB" = gr2)
grl

#seqinfo(gr1) <- Seqinfo(genome="hg38")
#can have names for each row



## -----------------------------------------------------------------------------
seqnames(gr1)
seqlevels(gr1)
ranges(gr1)
start(gr1)
end(gr1)
strand(gr1)
width(gr1)
length(gr1)
mcols(gr4)
mcols(gr4)$name


## -----------------------------------------------------------------------------
#data.frame-like

gr1[1,]
gr4[1, "name"]


# subset function
subset(gr1, strand == "+")


## -----------------------------------------------------------------------------

sp <- split(gr2, 1:2)
split(gr4, ~ name)

c(sp[[1]], sp[[2]])

stack(sp, index.var="name")



## -----------------------------------------------------------------------------
aggregate(gr2, score ~ strand, mean)


## -----------------------------------------------------------------------------
flank(gr1, 10, both=T)
shift(gr2, 5)
resize(gr1, 20)
c(gr1, gr3)
 reduce(c(gr1, gr3)) # combines the overlapping ranges

 reduce(c(gr1, gr3), ignore.strand=T)

## -----------------------------------------------------------------------------
#union(gr2, gr4)

#intersect(gr2, gr4)
#setdiff(gr2, gr4)

#hits <- findOverlaps(gr2, gr4, ignore.strand=TRUE)
#countOverlaps(gr2, gr4, ignore.strand=TRUE)
#subsetByOverlaps(gr2, gr4)



## -----------------------------------------------------------------------------
library("pasilla")
pasCts <- system.file("extdata",
                      "pasilla_gene_counts.tsv",
                      package="pasilla", mustWork=TRUE)
pasAnno <- system.file("extdata",
                       "pasilla_sample_annotation.csv",
                       package="pasilla", mustWork=TRUE)
cts <- as.matrix(read.csv(pasCts,sep="\t",row.names="gene_id"))
coldata <- read.csv(pasAnno, row.names=1)
coldata <- coldata[,c("condition","type")]

rownames(coldata) <- sub("fb", "", rownames(coldata))
cts <- cts[, rownames(coldata)]

library("DESeq2")
dds <- DESeqDataSetFromMatrix(countData = cts,
                              colData = coldata,
                              design = ~ condition)
dds
dds <- dds[1:200,]
dds <- DESeq(dds)
res <- results(dds)
res



