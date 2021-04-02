require(FuzzyNumbers)
require(class)
require(tidyverse)

#======================= fuzzification ====================================

fuzzification <- function(x){
  FN_param <- c(min(x),
                mean(x),
                max(x))


  tri_FN <- TrapezoidalFuzzyNumber(FN_param[1],
                                   FN_param[2],
                                   FN_param[2],
                                   FN_param[3])
  member_func <- evaluate(object = tri_FN,x)

  return(member_func)
}


#======================= fuzzy similiarity ====================================

fuzzy_similiar <- function(xFN,yFN){
  stack_FN <- rbind(xFN,yFN)
  colnames(stack_FN) <- NULL
  min_mf <- apply(stack_FN,2,min)
  max_mf <- apply(stack_FN,2,max)
  result <- sum(min_mf) / sum(max_mf)
  return(result)
}


#======================= Train Model ====================================

train_FLVQ <- function(data,response,
                       alpha=0.01,
                       beta=0.8,
                       max_iter=200){
if(!is.data.frame(data)){
  stop("data must be data.frame object")
}


predictors <- dta1 %>% select(-all_of(response))
y <- dta1 %>% pull(all_of(response))

if(!is.factor(y)){
  stop("response must be factor object")
}

codebook <- lvqinit(predictors,y)

fuzzy_predictors <- apply(predictors,1,fuzzification)


for (t in max_iter){

# preparing codebook
fuzzy_codebook <- apply(codebook$x,1,fuzzification)
iter_predictors <- seq(1:nrow(predictors))
iter_codebook <- seq(1:nrow(codebook$x))

# calculate fuzzy similarity
calc_similarity <-suppressMessages( map_dfc(iter_predictors,
              function(i){
                map_dbl(iter_codebook,
                        function(j){
                          fuzzy_similiar(fuzzy_predictors[,i],
                                         fuzzy_codebook[,j])
                        }
                )
                }
              )
)
final_similarity <- apply(calc_similarity,2,
                          which.max)
names(final_similarity) <- NULL

# decide winner or loser codebook
codebook_similarity_class <- unlist(map(final_similarity,~
                               codebook$cl[.x]))
codebook_similarity_predictors <- map_dfr(final_similarity,
                                          ~ as.data.frame(codebook$x)[.x,])
codebook_similarity <- cbind(codebook_similarity_predictors,
                                     "class_codebook"=
                               codebook_similarity_class,
                             "class"=y,
                             "index_codebook"=final_similarity
                             )
codebook_similarity <- codebook_similarity %>%
                        mutate(status=
                                 ifelse(class==class_codebook,"win","lose"))

winner_codebook <- codebook_similarity%>%
  filter(status == "win")

loser_codebook <- codebook_similarity%>%
  filter(status == "lose")

# FLVQ hyperparamter
alpha <- 0.999*alpha
beta <- 0.999*beta
theta <- 2-beta

# find optimal codebook
new_codebook <- codebook$x
new_codebook <- map(iter_predictors,function(i){
# winner codebook update
old_winner_codebook <- codebook$x[winner_codebook %>%
                             slice(i) %>%
                             pull(index_codebook),] %>%
                              as.numeric
new_codebook[winner_codebook %>%
               slice(i) %>%
               pull(index_codebook),] <- theta*(old_winner_codebook-alpha*(predictors %>%
                                                                             slice(i) %>%
                                                   as.numeric-old_winner_codebook))
# loser codebook update
old_loser_codebook <- codebook$x[loser_codebook %>%
                                    slice(i) %>%
                                    pull(index_codebook),] %>%
  as.numeric
new_codebook[loser_codebook %>%
               slice(i) %>%
               pull(index_codebook),] <- theta*(old_loser_codebook-alpha*(predictors %>%
                                                                             slice(i) %>%
                                                                             as.numeric-old_loser_codebook))
return(new_codebook)
})
new_codebook <- new_codebook[[length(new_codebook)]]
codebook$x <- new_codebook
}

result <- list("codebook"=codebook$x,
               "class_codebook"=codebook$cl,
               "response"=response)
class(result) <- "FLVQ"
return(result)
}
