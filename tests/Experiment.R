
data.frame(zip.train, )

is.01 <- zip.train[,1] %in % c(0,1)

data.list <- list(
  spam = list(
    features = as.matrix(spam[,1:57]),
    labels = ifelse(spam$spam == "spam", 1, 0)
  ),
  zip.train = list(
    features = zip.train[is.01, -1],
    labels = as.integer(zip.train[is,01, 1])
  )
  
)

n.folds <- 5

for(data.name in names(data.list)){
  data.set <- data.list[[data.name]]
  stopifnot(all(data.set$labels %in% c(0,1)))
  stopifnot(length(dat.set$labels) == nrow(data.set$features))
  set.seed(1)
  fold.vec <-  sample(rep(1:n.folds, l= nrow(data.set$features)))
   
}


####################### Data set 1: spam #######################





####################### Data set 2: SAheart ####################






####################### Data set 3: zip.train ##################





####################### Data set 4: prostate ###################





####################### Data set 5: ozone ######################
