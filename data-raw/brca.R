#Script formats raw breast cancer dataset (UCI ML repository) into brca dataset, for use in package

#Either load data directly from UCI ML website
breast.raw <- read.delim("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data",
                         sep = ",", head=FALSE)
write.csv(breast.raw, file = "data-raw/brca.csv")

#Or load data from local source, both are the same dataset
breast.raw  <- read.csv("data-raw/brca.csv")[,-1]

#Clean data
breast.raw <- breast.raw[,-1] #remove identifiers
breast.raw$V11 <- lapply(X = breast.raw$V11, FUN = function (x){ifelse(x == 2, "benign", "malignant")})
breast <- rbind(breast.raw[which(breast.raw[,"V11"]!="benign"), ], breast.raw[which(breast.raw[,"V11"]=="benign"), ])
brca <- breast[which(rowSums(breast == "?") == 0), ]
rownames(brca) <- 1:nrow(brca)
colnames(brca) <- c("clt", "ucs", "uch",
                    "mad", "ecs", "nuc", "chr", "ncl", "mit", "cla")

#Write to data file
devtools::use_data(brca, overwrite = TRUE)




