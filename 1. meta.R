library(openxlsx)
library(readxl)
library(dplyr)
library(meta)


#---- FUNCTION ---
str_split <- function(string){
  
  x <- gregexpr("-",string)[[1]]
  loc <- x[length(x)]
  n <- nchar(string)
  s1 <- as.numeric(substr(string, 1, loc-1))
  s2 <- as.numeric(substr(string, loc+1, n))
  output <- c(s1, s2)
  return(output)
}

tmp_choose_2 <- function(data){
  
  data$choose <- NA
  data$choose[is.na(data$mean)==F & is.na(data$mean_ci)==F] <- 1 #mean + mean_ci
  data$choose[is.na(data$mean)==F & is.na(data$mean_cri)==F & is.na(data$choose)] <- 2  #mean + mean_cri

  data$choose[is.na(data$median)==F & is.na(data$IQR)==F &is.na(data$choose) & !is.na(data$Sample_size)] <- 5  #median + IQR + n
  data$choose[is.na(data$median)==F & is.na(data$range)==F &is.na(data$choose) & !is.na(data$Sample_size)] <- 6  #median + RANGE +n

  return(data)
}

tmp_split_2 <- function(data_w){
  
  data_w$Sample_size <- as.numeric(data_w$Sample_size)  
  data_w$mean <- as.numeric(data_w$mean)  
  data_w$median <- as.numeric(data_w$median)  
  data_w$mean_ci_low <- NA
  data_w$mean_ci_up <- NA
  data_w$q1 <- NA
  data_w$q3 <- NA
  data_w$min <- NA
  data_w$max <- NA
  

  for(i in 1:nrow(data_w)){
    if(data_w[i,]$choose == 1){  #1: mean+mean_ci  
      data_w[i,]$mean_ci_low <- str_split(data_w[i,]$mean_ci)[1] 
      data_w[i,]$mean_ci_up <- str_split(data_w[i,]$mean_ci)[2]
    }
    
    if(data_w[i,]$choose == 2){   #2:mean+mean_cri
      data_w[i,]$mean_ci_low <- str_split(data_w[i,]$mean_cri)[1] 
      data_w[i,]$mean_ci_up <- str_split(data_w[i,]$mean_cri)[2]
    }
      
    if(data_w[i,]$choose == 5){  #5: median+IQR +n
      data_w[i,]$q1 <- str_split(data_w[i,]$IQR)[1]
      data_w[i,]$q3 <- str_split(data_w[i,]$IQR)[2] 
    }
      
    if(data_w[i,]$choose == 6){ #6: median+range +n
      data_w[i,]$min <- str_split(data_w[i,]$range)[1]
      data_w[i,]$max <- str_split(data_w[i,]$range)[2]
    }
    
  }
 
  return(data_w)
}



tmp_meta <- function(data, para_tmp, type_tmp, in_re, sub_tmp, xlim){
  output = list()
  
  if(!missing(type_tmp) & missing(in_re) & missing(sub_tmp)){ #only type
    
    data_ana <- data %>%
      filter(para == para_tmp, type == type_tmp) %>%
      arrange(mid_date)
    path <- paste0(para_tmp,"_", type_tmp, ".tiff")
    in_re = NA
    sub_tmp = NA
    
  }

  if(!missing(type_tmp) & !missing(in_re) & missing(sub_tmp)){ #type + in_re
    data_ana <- data %>%
      filter(para == para_tmp, type == type_tmp, Intrinsic_or_realized == in_re) %>%
      arrange(mid_date)
    path <- paste0(para_tmp,"_", type_tmp,"_", in_re, ".tiff")
    sub_tmp = NA
  }
  
 if(missing(type_tmp) & !missing(in_re) & !missing(sub_tmp)){  #sublineage + in_re
      data_ana <- data %>%
        filter(para == para_tmp, sublineage == sub_tmp, Intrinsic_or_realized == in_re) %>%
        arrange(mid_date)
      path <- paste0(para_tmp,"_", sub_tmp,"_", in_re, ".tiff")
      type_tmp = NA
 }
  
  if(missing(type_tmp) & missing(in_re) & !missing(sub_tmp)){  #sublineage
    data_ana <- data %>%
      filter(para == para_tmp, sublineage == sub_tmp) %>%
      arrange(mid_date)
    path <- paste0(para_tmp,"_", sub_tmp, ".tiff")
    in_re = NA
    type_tmp = NA
  }
  
   
 
  result <- metagen(TE = mean, studlab = Author_year, lower = mean_ci_low, upper = mean_ci_up,
                    median = median, q1 = q1, q3 = q3, min = min, max = max, 
                    sm = "MD", n.e = Sample_size, method.mean = "Luo",data = data_ana)
  

    tiff(path,
         width = 700, height = 1000)

    forest(result,col.square = "lightslategray", col.diamond = "maroon",
           random = result$random, common = F, pooled.totals = F,
           prediction = result$prediction, label.e = "",
           leftcols = c("studlab","effect", "ci"), rightcols = c( "w.random"),
           leftlabs = c("Study", para_tmp, "95% CI"), rightlabs = c( "Weight"),
           overall.hetstat = T, print.tau2 = F,
           studlab = T,text.random = "Random",
           smlab = " ", xlim = xlim)
    dev.off()
    
    data_pooled <- data.frame(type = type_tmp, para = para_tmp, value = result[["TE.random"]],
                              low = result[["lower.random"]], up = result[["upper.random"]],
                              n_record = nrow(data_ana),
                              Intrinsic_or_realized = in_re,
                              sub = sub_tmp
                              )
    
    output[[1]] <- data_pooled
    
    return(output)
  
}

#------
data_raw <- read_excel("code/public/DATA_RAW.xlsx")

#only choose==1,2,5,6 include for meta-analysis
data_meta <- data_raw %>% 
  tmp_choose_2(.) %>% 
  filter(choose %in% c(1,2,5,6)) %>%
  tmp_split_2(.)

#-----
DT1 <- tmp_meta(data = data_meta, para_tmp = "IP", type_tmp = "Ancestral lineage", xlim = c(0,13.5))
DT2 <- tmp_meta(data = data_meta, para_tmp = "SI", type_tmp = "Ancestral lineage",in_re = "realized", xlim = c(-5,16.5))
DT3 <- tmp_meta(data = data_meta, para_tmp = "GT", type_tmp = "Ancestral lineage",in_re = "realized", xlim = c(0,8))

DT4 <- tmp_meta(data = data_meta, para_tmp = "IP", type_tmp = "Alpha", xlim = c(0,8))
DT5 <- tmp_meta(data = data_meta, para_tmp = "IP", type_tmp = "Beta", xlim = c(0,8))
DT6 <- tmp_meta(data = data_meta, para_tmp = "IP", type_tmp = "Delta", xlim = c(0,8))
DT7 <- tmp_meta(data = data_meta, para_tmp = "IP", type_tmp = "Omicron", xlim = c(0,6))

DT8 <- tmp_meta(data = data_meta, para_tmp = "SI", type_tmp = "Alpha", in_re = "realized", xlim = c(0,5))
DT9 <- tmp_meta(data = data_meta, para_tmp = "SI", type_tmp = "Delta", in_re = "realized", xlim = c(0,9))
DT10 <- tmp_meta(data = data_meta, para_tmp = "SI", type_tmp = "Omicron", in_re = "realized", xlim = c(0,6))

DT11 <- tmp_meta(data = data_meta, para_tmp = "GT", type_tmp = "Alpha", in_re = "realized", xlim = c(2,6))
DT12 <- tmp_meta(data = data_meta, para_tmp = "GT", type_tmp = "Delta", in_re = "realized", xlim = c(0,6))
DT13 <- tmp_meta(data = data_meta, para_tmp = "GT", type_tmp = "Omicron", in_re = "realized", xlim = c(0,6))

DT14 <- tmp_meta(data = data_meta, para_tmp = "GT", type_tmp = "Alpha", in_re = "intrinsic", xlim = c(3,7))
DT15 <- tmp_meta(data = data_meta, para_tmp = "GT", type_tmp = "Delta", in_re = "intrinsic", xlim = c(0,8))
DT16 <- tmp_meta(data = data_meta, para_tmp = "GT", type_tmp = "Omicron", in_re = "intrinsic", xlim = c(3,9))

#sublineage
DT17 <- tmp_meta(data = data_meta, para_tmp = "IP", sub_tmp = "BA.1", xlim = c(0,8))
DT18 <- tmp_meta(data = data_meta, para_tmp = "IP", sub_tmp = "BA.2", xlim = c(0,8))
DT19 <- tmp_meta(data = data_meta, para_tmp = "IP", sub_tmp = "BA.5", xlim = c(0,8))
DT20 <- tmp_meta(data = data_meta, para_tmp = "IP", sub_tmp = "unspecified", xlim = c(0,8))


DT21 <- tmp_meta(data = data_meta, para_tmp = "SI", sub_tmp = "BA.1", in_re = "realized", xlim = c(0,6))
DT22 <- tmp_meta(data = data_meta, para_tmp = "SI", sub_tmp = "BA.2", in_re = "realized", xlim = c(0,8))
DT23 <- tmp_meta(data = data_meta, para_tmp = "SI", sub_tmp = "BA.4", in_re = "realized", xlim = c(0,8))
DT24 <- tmp_meta(data = data_meta, para_tmp = "SI", sub_tmp = "BA.5", in_re = "realized", xlim = c(0,5))
DT25 <- tmp_meta(data = data_meta, para_tmp = "SI", sub_tmp = "unspecified", in_re = "realized", xlim = c(0,8))

DT26 <- tmp_meta(data = data_meta, para_tmp = "GT", sub_tmp = "BA.1", in_re = "realized", xlim = c(0,5))
DT27 <- tmp_meta(data = data_meta, para_tmp = "GT", sub_tmp = "BA.5", in_re = "realized", xlim = c(0,5))
DT28 <- tmp_meta(data = data_meta, para_tmp = "GT", sub_tmp = "BA.1", in_re = "intrinsic", xlim = c(4,10))
#----- pooled data -----------
data_res <- bind_rows(DT1[[1]], DT2[[1]], DT3[[1]], DT4[[1]], DT5[[1]], DT6[[1]],
                      DT7[[1]], DT8[[1]], DT9[[1]], DT10[[1]], DT11[[1]], DT12[[1]],
                      DT13[[1]], DT14[[1]], DT15[[1]], DT16[[1]], DT17[[1]], DT18[[1]],
                      DT19[[1]], DT20[[1]], DT21[[1]], DT22[[1]], DT23[[1]], DT24[[1]],
                      DT25[[1]], DT26[[1]], DT27[[1]], DT28[[1]]) 

data_res_1 <- data_res %>% 
  mutate(para = factor(data_res$para,levels=c("IP","SI","GT")),
         type = factor(data_res$type,levels=c("Ancestral lineage","Beta","Alpha","Delta","Omicron")),
         value = round(value, 2),
         low = round(low, 2),
         up = round(up, 2)
  ) %>% 
  arrange(para, type) 


write.xlsx(data_res_1, "META_RESULT.xlsx")


#--data for wilcoxon test
result_all <- metagen(TE = mean, studlab = Author_year, lower = mean_ci_low, upper = mean_ci_up,
                  median = median, q1 = q1, q3 = q3, min = min, max = max, 
                  sm = "MD", n.e = Sample_size, method.mean = "Luo",data = data_meta)

data_test <- data_meta %>% 
  mutate(ana_TE = as.numeric(sprintf("%0.2f", result_all[["TE"]])))


write.xlsx(data_test, "code/public/DATA_TEST.xlsx")




