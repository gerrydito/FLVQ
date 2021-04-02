source('R//train_FLVQ.R')
source('R//predictFLVQ.R')
library(rsample)
dta1 <- iris
index_train1 <- initial_split(dta1,prop = 0.8,
                              strata = Species )
train1 <- training(index_train1)
test1 <- testing(index_train1)

set.seed(797)
mod <- train_FLVQ(data = train1,
                  response = "Species")

mean(predict(mod,newdata=test1)==test1$Species)
