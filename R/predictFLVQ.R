predict.FLVQ=function(model,newdata){
  if(class(model)!="FLVQ"){
    "class model must be FLVQ"
  }
  predictors <- newdata %>% select(-all_of(model$response))
  y <- dta1 %>% pull(all_of(model$response))

  fuzzy_predictors <- apply(predictors,1,fuzzification)

  # preparing codebook
  fuzzy_codebook <- apply(model$codebook,1,fuzzification)
  iter_predictors <- seq(1:nrow(predictors))
  iter_codebook <- seq(1:nrow(model$codebook))

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
                                            model$class_codebook[.x]))
  return(codebook_similarity_class)

}
