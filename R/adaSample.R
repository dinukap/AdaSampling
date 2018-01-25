#' Implementation of AdaSampling for positive unlabelled and label noise
#' learning.
#'
#' \code{adaSample()} applies the AdaSampling procedure to reduce noise
#' in the training set, and subsequently trains a classifier from
#' the new training set. For each row (observation) in the test set, it
#' returns the probabilities of it being a positive ("P) or negative
#' ("N") instance, as a two column data frame.
#'
#' \code{adaSample()} is an adaptive sampling-based noise reduction method
#' to deal with noisy class labelled data, which acts as a wrapper for
#' traditional classifiers, such as support vector machines,
#' k-nearest neighbours, logistic regression, and linear discriminant
#' analysis.
#'
#' This process is used to build up a noise-minimized training set
#' that is derived by iteratively resampling the training set,
#' (\code{train}) based on probabilities derived after its classification.
#'
#' This sampled training set is then used to train a classifier, which
#' is then executed on the test set. \code{adaSample()} returns a series of
#' predictions for each row of the test set.
#'
#' Note that this function does not evaluate the quality of the model
#' and thus does not compare its output to true values of the test set.
#' To assess please see \code{adaBenchmark()}.
#'
#'
#' @section References:
#' Yang, P., Liu, W., Yang. J. (2016) Positive unlabeled learning via wrapper-based
#' adaptive sampling. \emph{International Joint Conferences on Artificial Intelligence Organization.}
#'
#' @seealso allows you to point to other useful resources, either on the web, \url{http://www.r-project.org}
#'
#' @param train.mat a rectangular matrix or data frame that can be
#' coerced to a matrix, containing the
#' features of training data, without class labels. Rownames (possibly
#' containing unique identifiers) will be ignored.
#' @param test.mat a rectangular matrix or data frame that can be
#' coerced to a matrix, containg the test
#' set, without class labels. Rownames are ignored.
#' @param cls a numeric vector of class labels for the training set, must be in
#' the same order and of the same length as \code{train.mat}. Labels must
#' be 1, for positive observations, and 0 for negative observations.
#' @param classifier the type of classifier to be used, options are
#' support vector machine, \code{"svm"}, k-nearest neighbour,
#' \code{"knn"}, logistic regression \code{"logit"}, or
#' linear discriminant analysis \code{"lda"}.
#' @param s sets the seed.
#' @param C sets how many times to run the classifier, C>1 induces an
#' ensemble learning model.
#' @param sampleFactor provides a control on the sample size for resampling.
#' @export

adaSample <- function(train.mat, test.mat, cls, classifier="knn", s=1, C=1, sampleFactor=1) {

  #if(!is.vector(cls)) {stop("cls is not a vector")} #first check if cls is a vector

  if(nrow(train.mat) != length(cls))  {stop("train.mat and cls are of different lengths")}

  if(ncol(train.mat) != ncol(test.mat)) {stop("train.mat and test.mat do not have the same number of columns")}

  if(length(setdiff(cls, c(0, 1))) != 0) {stop("cls must only contain 1s and 0s as numeric values")}


  user.inputted.rownames <- rownames(train.mat) #The only reason these indexes are needed is to help
  #match PL.list and DL.list together.
  rownames(train.mat) <- paste("row", seq(1:nrow(train.mat)), sep = "")

  Pl.list <- paste("row", (which(cls==1)), sep ="") #changed to mimic real usage of cls (vector of unnamed labels)
  Dl.list <- paste("row", (which(cls==0)), sep ="")


  # initialize pws sampling probablity (does pws stand for probability weighted sampling?)
  pos.probs <- rep(1, length(Pl.list))
  una.probs <- rep(1, length(Dl.list))
  names(pos.probs) <- Pl.list
  names(una.probs) <- Dl.list


  #this is the pws() function
  pws <- function(Pl.list, Dl.list, dat, test=NULL, pos.probs=NULL, una.probs=NULL, classifier="knn", sampleFactor, seed) {
    set.seed(seed);

    positive.train <- c()
    positive.cls <- c()

    # bootstrap sampling to build the positive training set (labeled as '1')
    idx.pl <- unique(sample(x=Pl.list, size=sampleFactor*length(Pl.list), replace=TRUE, prob=pos.probs[Pl.list]))
    positive.train <- dat[idx.pl,]
    positive.cls <- rep("P", nrow(positive.train))

    # bootstrap sampling to build the "unannotate" or "negative" training set (labeled as '0')
    idx.dl <- unique(sample(x=Dl.list, size=sampleFactor*length(Dl.list), replace=TRUE, prob=una.probs[Dl.list]))
    unannotate.train <- dat[idx.dl,]
    unannotate.cls <- rep("N", nrow(unannotate.train))

    # combine data
    train.sample <- rbind(positive.train, unannotate.train)
    rownames(train.sample) <- NULL;
    cls <- as.factor(c(positive.cls, unannotate.cls))

    # training svm classifier
    if (classifier == "svm") {

      model.svm <- e1071::svm(train.sample, cls, probability=TRUE, scale=TRUE);
      svm.pred <- c();
      if (is.null(test)) {
        svm.pred <- predict(model.svm, dat, decision.values=TRUE, probability=TRUE);
      } else {
        svm.pred <- predict(model.svm, test, decision.values=TRUE, probability=TRUE);
      }
      return(attr(svm.pred,"probabilities"));

    }
    else if (classifier == "knn") { #this method will use knn algorithm
      # training knn classifier

      if (is.null(test)) {
        knn.fit <- class::knn(train.sample, dat, cl=cls, k=5, prob=TRUE) #why did we have to augment the training set
        p <- attr(knn.fit, "prob")
        idx <- which(knn.fit == "N")
        p[idx] <- 1- p[idx]
        knn.pred <- cbind(p, 1 - p)
        colnames(knn.pred) <- c("P", "N")
        rownames(knn.pred) <- rownames(dat)
        return(knn.pred)
      } else {
        test.mat <- test
        rownames(test.mat) <- NULL
        knn.fit <- class::knn(train.sample, test.mat, cl=cls, k=5, prob=TRUE)

        p <- attr(knn.fit, "prob")
        idx <- which(knn.fit == "N")
        p[idx] <- 1- p[idx]
        knn.pred <- cbind(p, 1 - p)
        colnames(knn.pred) <- c("P", "N")
        rownames(knn.pred) <- rownames(test)
        return(knn.pred)
      }
    }
    else if (classifier == "logit") {
      logit.model <- glm(cls~., family=binomial(link='logit'), data=data.frame(train.sample, cls))
      if (is.null(test)) {
        p <- predict(logit.model, newdata=data.frame(dat), type='response')
        logit.pred <- cbind(p, 1-p)
        colnames(logit.pred) <- c("P", "N")
        rownames(logit.pred) <- rownames(dat)
        return(logit.pred)
      } else {
        test.mat <- data.frame(test)
        rownames(test.mat) <- NULL
        colnames(test.mat) <- colnames(dat)
        p <- predict(logit.model, newdata=test.mat, type='response')
        logit.pred <- cbind(p, 1-p)
        colnames(logit.pred) <- c("P", "N")
        rownames(logit.pred) <- rownames(test)
        return(logit.pred)
      }
    }
    else if (classifier == "lda") {
      lda.model <- lda(cls~., data=data.frame(train.sample, cls))
      if (is.null(test)) {
        lda.pred <- predict(lda.model, data.frame(dat))$posterior
        colnames(lda.pred) <- c("N", "P")
        rownames(lda.pred) <- rownames(dat)
        return(lda.pred)
      } else {
        test.mat <- data.frame(test)
        rownames(test.mat) <- NULL
        colnames(test.mat) <- colnames(dat)
        lda.pred <- predict(lda.model, test.mat)$posterior
        colnames(lda.pred) <- c("N", "P")
        rownames(lda.pred) <- rownames(test)
        return(lda.pred)
      }
    }
  }


  i <- 0
  while (i < 5) {
    # update count
    i <- i + 1
    # prediction using probability weighted sampling
    model <- pws(Pl.list=Pl.list, Dl.list=Dl.list, dat=train.mat, pos.probs=pos.probs,    #train.mat is features of training data outside fold
                 una.probs=una.probs, seed=i, classifier=classifier, sampleFactor=sampleFactor)

    # update probability lists
    pos.probs <- model[Pl.list,"P"] #what are these probabilities - the probability of being right? Is this the posterior prob?
    una.probs <- model[Dl.list,"N"]
  }

  pred <- pws(Pl.list=Pl.list, Dl.list=Dl.list, dat=train.mat, test=test.mat,
              pos.probs=pos.probs, una.probs=una.probs, seed=s, classifier=classifier, sampleFactor=sampleFactor)

  # if r is greater than 1, create an ensemble
  if (C > 1){
    for (j in 2:C){
      pred <- pred + pws(Pl.list=Pl.list, Dl.list=Dl.list, dat=train.mat, test=test.mat,
                         pos.probs=pos.probs, una.probs=una.probs, seed=j, classifier=classifier, sampleFactor=sampleFactor)
    }
    pred <- pred/C
  }

  return(pred)
}
