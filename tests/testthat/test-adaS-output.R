context("adaS-outputdata")

features.brca <- apply(X = brca[,-10], MARGIN = 2, FUN = as.numeric)
labels.brca <- sapply(X = brca$cla, FUN = function(x) {ifelse(x == "malignant", 1, 0)})

set.seed(1)
train.ind <- sort(sample(nrow(brca), (4*nrow(brca))/5))
test.ind <- seq(1:nrow(brca))[-train.ind]

train.mat <- features.brca[train.ind,]
test.mat <- features.brca[test.ind, ]

train.cls <- labels.brca[train.ind]

#adding noise
pos <- which(train.cls == 1)
neg <- which(train.cls == 0)
train.noisy.cls <- train.cls
train.noisy.cls[sample(pos, floor(length(pos) * 0.4))] <- 0
train.noisy.cls[sample(neg, floor(length(neg) * 0.2))] <- 1

testthat::test_that("output data has same no of rows as input mats", {
  expect_equal(nrow(test.mat),
               {nrow(adaSample(train.mat = train.mat, test.mat = test.mat, cls = train.noisy.cls))}
               )

})
