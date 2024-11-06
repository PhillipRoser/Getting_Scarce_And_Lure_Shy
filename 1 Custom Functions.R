library(dplyr)
library(ggplot2)

####
#### Define Colors
####


Color_Sel = "#00AFBB"
  Color_Wer = "#FC4E07"
    Color_Umm = "#E7B800"
      Color_MPA = "#08931d"
        Color_OA  = "#619CCF"

# # #     # # # # # #       #   # # #   #       # # # # # #       # # # # #       # #       # #       # # # # # # # # # #   #       # 
# # #     #       # # #     # #       # #       # #       #     #         #       # # #   # # # #   # # #       # #     #     #   #   
# # #     # # # # # #   #   # #       # #       # # # # # #       # # #   #       # #   #   # #   #   # # # # # # # # # #       #     
# # #     #       # #     # # #       #   #   #   #       #             # #       # #       # #       # #       # #   #         #     
# # #     #       # #       #   # # #       #     #       #     # # # #     # # #   #       # #       # #       # #     #       # 

extract_Anova <- function(anova_table, round = TRUE, digits = 3){
  for (i in 1:nrow(anova_table)) {
    variable <- rownames(anova_table)[i]
    p_value <- format(anova_table$"Pr(>Chisq)"[i], scientific = FALSE, digits = digits) %>% as.numeric()
    df <- anova_table$"Df"[i]
    chisq <- format(anova_table$"Chisq"[i], scientific = FALSE, digits = digits) %>% as.numeric()
    
    if(round){
      p_value <- ifelse(p_value < 0.001, "< 0.001", paste0("= ", format(round(p_value, digits), scientific = F)))
      chisq   <- round(chisq, 2)
    }
    
    # output <- paste0(variable, " (p ", p_value, ", df = ", df, ", chisq = ", chisq, ")")
    output <- paste0(variable, " (χ2 = ", chisq, ", df = ", df, ", p ", p_value, ")")
    print(output)
  }
}

# # #     # # # #   #       # #       # # # # #   # # # # # #       #   # # # #       # # # # #       # #       # #       # # # # # # # # # #   #       # 
# # #     #         # #   # # # #   # # #         #       # # #     # #             #         #       # # #   # # # #   # # #       # #     #     #   #   
# # #     # # # #   #   #   # #   #   # # # # #   # # # # # #   #   #   # # #         # # #   #       # #   #   # #   #   # # # # # # # # # #       #     
# # #     #         #       # #       # #         #       # #     # #         #             # #       # #       # #       # #       # #   #         #     
# # #     # # # #   #       # #       # # # # #   #       # #       # # # # #       # # # #     # # #   #       # #       # #       # #     #       #     


extract_emmeans <- function(emmeans_obj, round = TRUE, round_by = 3, print_table = FALSE) {
  emmeans_data <- emmeans_obj$emmeans %>% as.data.frame()
  
  for (i in 1:nrow(emmeans_data)) {
    
    factor_level <- emmeans_data[i,1]
    
    if(names(emmeans_data)[2] == "prob"){
      prediction <- emmeans_data$prob[i] 
      type = "prob"
    }else if(names(emmeans_data)[2] == "rate"){
      prediction <- emmeans_data$rate[i] 
      type = "rate"
    }else if(names(emmeans_data)[2] == "emmean"){
      prediction <- emmeans_data$emmean[i]
      type = "emmean"
    }else if(names(emmeans_data)[3] == "emmean"){ # TRUE when two arguments after pairwise ~
      prediction <- emmeans_data$emmean[i]
      type = "emmean"
    }else (stop("Please adjust function or use only one argument after pairwise ~"))
    
    
    if(type %in% c("rate", "prob")){
      asymp_LCL <- emmeans_data$asymp.LCL[i]
      asymp_UCL <- emmeans_data$asymp.UCL[i]
    }else{
      asymp_LCL <- emmeans_data$lower.CL[i]
      asymp_UCL <- emmeans_data$upper.CL[i]
    }
    
    if (round) {
      prediction <- ifelse(type == "prob", paste0(round(prediction * 100), "%"), round(prediction, round_by))
      asymp_LCL <- round(asymp_LCL, round_by)
      asymp_UCL <- round(asymp_UCL, round_by)
    }
    
    output <- paste0(prediction, " [", asymp_LCL, " - ", asymp_UCL, "; 95% CL]")
    if(!print_table) print(paste(factor_level, output))
    
    if(print_table){
      if(i == 1) cat("level\tprediction\t95%LCL\t95%UCL\n")
      cat(factor_level, "\t", prediction, "\t", asymp_LCL, "\t", asymp_UCL, "\n")
    }
  }
}


# # #     # # # #   #       # # # # # # # # # #             #       # # # # #   # # # # # #       #                     # # # # # # # #   
# # #     #           #   #       #     #     #             # #   # # #         #       # # #     #                   #         #       # 
# # #     # # # #       #         #     # # # #             #   #   # # # # #   # # # # # #   #   #       # # #         # # #   #       # 
# # #     #           #   #       #     #   #               #       # #         #       # #     # #                           # #       # 
# # #     # # # #   #       #     #     #     #     #       #       # # # # #   #       # #       #                   # # # #   # # # #   


extract_mean_sd_summarized <- function(summarized_object, round_by = 1, convert.decimals = FALSE){
  
  df <- summarized_object %>% as.data.frame() %>% mutate(across(where(is.factor), as.character))
  
  if( sum(names(df) %in% c("mean", "sd")) != 2 ){cat(crayon::red(crayon::bold("Please provide the columns 'mean' and 'sd'\n"))); stop()}
  
  other_cols <- df[,which(!names(df) %in% c("mean", "sd"))] %>% as.data.frame()
  
  for(i in 1:nrow(df)){
    for(j in 1:ncol(other_cols)){cat(other_cols[i,j], " ")}
    mean.i <- round(df$mean[i], round_by)
    sd.i   <- round(df$sd[i], round_by)
    if(convert.decimals){
      mean.i <- sprintf("%.1e", mean.i)
      sd.i   <- sprintf("%.1e", sd.i)
    }
    cat("\t:", mean.i, "±", sd.i, "unit (mean ± SD)","\n")
  }
}


# # #     # # # #   #       # # # # # # # # # #             # # # #   #       # #       # # # # #   # # # # # #       #   # # # #     # # # #   # # # # # # # # # #   # # #     # # #   
# # #     #           #   #       #     #     #             #         # #   # # # #   # # #         #       # # #     # #             #     #   #       #     #         #     #       # 
# # #     # # # #       #         #     # # # #             # # # #   #   #   # #   #   # # # # #   # # # # # #   #   #   # # #       # # # #   # # # # #     #         #     #       # 
# # #     #           #   #       #     #   #               #         #       # #       # #         #       # #     # #         #     #   #     #       #     #         #     #       # 
# # #     # # # #   #       #     #     #     #     #       # # # #   #       # #       # # # # #   #       # #       # # # # #       #     #   #       #     #       # # #     # # #   

###
### Extracet post Hoc Differences
###

extract_emmeans_ratio <- function(emmeans_object, round = 3){
  
  obj <- as.data.frame(emmeans_object$contrasts)
  
  if(!"ratio" %in% names(obj)){
    warning("There is no column called ratio\n")
    print(names(obj))
    names(obj)[which(names(obj)=="estimate")] <- "ratio"
  }
  
  for(i in 1:nrow(obj)){
    p.value <- ifelse(obj[i,]$p.value < 0.001, "< 0.001", paste("=", as.character(round(obj[i,]$p.value, round))))
    obj$contrast[i] %>% print()
    cat("\t(emmeans, ratio = ", round(obj[i,]$ratio, round), " ± ", round(obj[i,]$SE, round), " SE" , ", p ",p.value ,")\n", sep = "")
  }
}


# # #     #       #   # # #   # # # #   # # # #       # # # # # # # # # # # # # #   #         # # # #   
# # #     #       # #       # #     #   #       #         #     #       # #       # #         #         
# # #     #   #   # #       # # # # #   #       #         #     # # # # # # # # #   #         # # # #   
# # #     # #   # # #       # #   #     #       #         #     #       # #       # #         #         
# # #     #       #   # # #   #     #   # # # #           #     #       # # # # #   # # # # # # # # #   


word_table <- function(data, sep = "\t", round_at = 4, select_cols = F){
  
  # Round p-value 
  if(any(grepl("p.value", names(data)))){
    data <- data %>% mutate(p.value = ifelse(round(p.value, 4) == 0, "< 0.0001", sprintf("%.4f", p.value)))
  }
  
  # Round other numeric variables
  data <- data %>% dplyr::mutate_if(is.numeric, ~round(., digits = round_at))
  
  if(select_cols) data <- data %>% dplyr::select(effect, term, estimate, std.error, p.value)
  
  for(h in 1:length(names(data))){cat(names(data[h])) ; if(h < length(names(data))){cat(sep)}} ; cat("\n")
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      cat(as.character(data[i,j])) ; if(j < ncol(data)){cat(sep)} 
    } 
    cat("\n")
  }
}



# # #     #       # # # # # #   # # # # #       # # # # # # # # # # # # # # # #     #       # # # # #     # # #   # # # # # # # # #   # # # #   
# # #     #       # #       # #         #       #     #     #       # #             #       # #     #       #         #     #         #     #   
# # #     # # # # # # # # # #   # # #   # # # # #     #     # # # # # #   # # #     #   #   # # # # #       #         #     # # # #   # # # #   
# # #     #       # #       #         # #       #     #     #       # #       #     # #   # # #   #         #         #     #         #   #     
# # #     #       # #       # # # # #   #       #     #     #       # # # # #       #       # #     #     # # #       #     # # # #   #     #  

# write_hashtag("hashtag")

hashtag_writer <- function(word){
  
  
  a <- matrix(c("#", "#", "#", "#", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#"), nrow = 5, byrow = TRUE)
  
  b <- matrix(c("#", "#", "#", "#", " ",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", " ",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  c <- matrix(c("#", "#", "#", "#", " ",
                "#", " ", " ", " ", " ",
                "#", " ", " ", " ", " ",
                "#", " ", " ", " ", " ",
                "#", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  d <- matrix(c("#", "#", "#", "#", " ",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  e <- matrix(c("#", "#", "#", "#", " ",
                "#", " ", " ", " ", " ",
                "#", "#", "#", "#", " ",
                "#", " ", " ", " ", " ",
                "#", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  f <- matrix(c("#", "#", "#", "#", "#",
                "#", " ", " ", " ", " ",
                "#", "#", "#", "#", " ",
                "#", " ", " ", " ", " ",
                "#", " ", " ", " ", " "), nrow = 5, byrow = TRUE)
  
  g <- matrix(c("#", "#", "#", "#", "#",
                "#", " ", " ", " ", " ",
                "#", " ", "#", "#", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  h <- matrix(c("#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#"), nrow = 5, byrow = TRUE)
  
  i <- matrix(c(" ", "#", "#", "#", " ",
                " ", " ", "#", " ", " ",
                " ", " ", "#", " ", " ",
                " ", " ", "#", " ", " ",
                " ", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  j <- matrix(c(" ", " ", " ", " ", "#",
                " ", " ", " ", " ", "#",
                " ", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  k <- matrix(c("#", " ", " ", "#", " ",
                "#", " ", "#", " ", " ",
                "#", "#", " ", " ", " ",
                "#", " ", "#", " ", " ",
                "#", " ", " ", "#", " "), nrow = 5, byrow = TRUE)
  
  l <- matrix(c("#", " ", " ", " ", " ",
                "#", " ", " ", " ", " ",
                "#", " ", " ", " ", " ",
                "#", " ", " ", " ", " ",
                "#", "#", "#", "#", "#"), nrow = 5, byrow = TRUE)
  
  m <- matrix(c("#", " ", " ", " ", "#",
                "#", "#", " ", "#", "#",
                "#", " ", "#", " ", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#"), nrow = 5, byrow = TRUE)
  
  n <- matrix(c("#", " ", " ", " ", "#",
                "#", "#", " ", " ", "#",
                "#", " ", "#", " ", "#",
                "#", " ", " ", "#", "#",
                "#", " ", " ", " ", "#"), nrow = 5, byrow = TRUE)
  
  o <- matrix(c(" ", "#", "#", "#", " ",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                " ", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  p <- matrix(c("#", "#", "#", "#", " ",
                "#", " ", " ", "#", " ",
                "#", "#", "#", "#", " ",
                "#", " ", " ", " ", " ",
                "#", " ", " ", " ", " "), nrow = 5, byrow = TRUE)
  
  q <- matrix(c(" ", "#", "#", " ", " ",
                "#", " ", " ", "#", " ",
                "#", " ", "#", "#", " ",
                "#", " ", " ", "#", " ",
                " ", "#", "#", " ", "#"), nrow = 5, byrow = TRUE)
  
  r <- matrix(c("#", "#", "#", "#", " ",
                "#", " ", " ", "#", " ",
                "#", "#", "#", "#", " ",
                "#", " ", "#", " ", " ",
                "#", " ", " ", "#", " "), nrow = 5, byrow = TRUE)
  
  s <- matrix(c(" ", "#", "#", "#", "#",
                "#", " ", " ", " ", " ",
                " ", "#", "#", "#", " ",
                " ", " ", " ", " ", "#",
                "#", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  t <- matrix(c("#", "#", "#", "#", "#",
                " ", " ", "#", " ", " ",
                " ", " ", "#", " ", " ",
                " ", " ", "#", " ", " ",
                " ", " ", "#", " ", " "), nrow = 5, byrow = TRUE)
  
  u <- matrix(c("#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                " ", "#", "#", "#", " "), nrow = 5, byrow = TRUE)
  
  v <- matrix(c("#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                " ", "#", " ", "#", " ",
                " ", " ", "#", " ", " "), nrow = 5, byrow = TRUE)
  
  w <- matrix(c("#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", " ", "#", " ", "#",
                "#", "#", " ", "#", "#",
                "#", " ", " ", " ", "#"), nrow = 5, byrow = TRUE)
  
  x <- matrix(c("#", " ", " ", " ", "#",
                " ", "#", " ", "#", " ",
                " ", " ", "#", " ", " ",
                " ", "#", " ", "#", " ",
                "#", " ", " ", " ", "#"), nrow = 5, byrow = TRUE)
  
  y <- matrix(c("#", " ", " ", " ", "#",
                " ", "#", " ", "#", " ",
                " ", " ", "#", " ", " ",
                " ", " ", "#", " ", " ",
                " ", " ", "#", " ", " "), nrow = 5, byrow = TRUE)
  
  z <- matrix(c("#", "#", "#", "#", "#",
                " ", " ", " ", " ", "#",
                " ", " ", " ", "#", " ",
                " ", " ", "#", " ", " ",
                "#", "#", "#", "#", "#"), nrow = 5, byrow = TRUE)
  
  ### Numbers
  
  Q <- matrix(c("#", "#", "#", "#", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", "#"), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for 1
  W <- matrix(c(" ", " ", "#", " ", " ",
                " ", "#", "#", " ", " ",
                " ", " ", "#", " ", " ",
                " ", " ", "#", " ", " ",
                "#", "#", "#", "#", "#"), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for 2
  E <- matrix(c("#", "#", "#", "#", "#",
                " ", " ", " ", " ", "#",
                "#", "#", "#", "#", "#",
                "#", " ", " ", " ", " ",
                "#", "#", "#", "#", "#"), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for 3
  R <- matrix(c("#", "#", "#", "#", "#",
                " ", " ", " ", " ", "#",
                "#", "#", "#", "#", "#",
                " ", " ", " ", " ", "#",
                "#", "#", "#", "#", "#"), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for 4
  T <- matrix(c("#", " ", " ", " ", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", "#",
                " ", " ", " ", " ", "#",
                " ", " ", " ", " ", "#"), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for 5
  Z <- matrix(c("#", "#", "#", "#", "#",
                "#", " ", " ", " ", " ",
                "#", "#", "#", "#", "#",
                " ", " ", " ", " ", "#",
                "#", "#", "#", "#", "#"), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for 6
  U <- matrix(c("#", "#", "#", "#", "#",
                "#", " ", " ", " ", " ",
                "#", "#", "#", "#", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", "#"), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for 7
  I <- matrix(c("#", "#", "#", "#", "#",
                " ", " ", " ", " ", "#",
                " ", " ", " ", "#", " ",
                " ", " ", "#", " ", " ",
                " ", " ", "#", " ", " "), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for 8
  O <- matrix(c("#", "#", "#", "#", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", "#"), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for 9
  P <- matrix(c("#", "#", "#", "#", "#",
                "#", " ", " ", " ", "#",
                "#", "#", "#", "#", "#",
                " ", " ", " ", " ", "#",
                "#", "#", "#", "#", "#"), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for .
  L <- matrix(c( " ", " ", " ", 
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", " ", " ",
                 " ", "#", " "), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for :
  Ö <- matrix(c(" ", " ", " ",
                " ", "#", " ",
                " ", " ", " ",
                " ", "#", " ",
                " ", " ", " "), 
              nrow = 5, byrow = TRUE)
  
  # Create matrix for -
  Ä <- matrix(c(" ", " ", " ", " ", " ",
                " ", " ", " ", " ", " ",
                " ", "#", "#", "#", " ",
                " "," ", " ", " ", " ",
                " "," ", " ", " ", " "),
              nrow = 5, byrow = TRUE)
  
  
  ### Special Characters
  
  space <- matrix(c(" ", " ",
                    " ", " ",
                    " ", " ",
                    " ", " ",
                    " ", " "), nrow = 5, byrow = TRUE)
  
  
  
  ####
  
  word_df <-  matrix(c("#", "#", "#", " ", " ",
                       "#", "#", "#", " ", " ",
                       "#", "#", "#", " ", " ",
                       "#", "#", "#", " ", " ",
                       "#", "#", "#", " ", " "), nrow = 5, byrow = TRUE)
  
  for(letters in 1:nchar(word)){
    
    letter.i <- substr(word, letters, letters)
    
    if(letter.i == " ") letter.i <- "space"
    
    add_word <- get(letter.i)
    
    word_df <- cbind(word_df, add_word )
  }
  
  for(i in 1:nrow(word_df)) cat(word_df[i,], "\n")
}







