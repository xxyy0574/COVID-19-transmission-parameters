library(openxlsx)
library(dplyr)


data_raw <- read_excel("code/public/DATA_TEST.xlsx") %>%
  mutate(type = ifelse(!is.na(sublineage), sublineage, type))

data_pool <- read.xlsx("code/public/META_RESULT.xlsx")  %>%
  mutate(type = ifelse(!is.na(sub), sub, type))

#--------FUNCTION pvalue
pvalue <- function(para_tmp, type_0, type_1, in_re){
  if(missing(in_re)){
  data_0 <- data_raw %>%
    filter(para == para_tmp, type == type_0)
  
  data_1 <- data_raw %>%
    filter(para == para_tmp, type == type_1)
  
  #----------pooled -----
  data_pool_0 <- data_pool %>%
    filter(para == para_tmp, type == type_0)
  
  data_pool_1 <- data_pool %>%
    filter(para == para_tmp, type == type_1)
  
  in_re = NA
  
   }else{
     data_0 <- data_raw %>%
       filter(para == para_tmp, type == type_0, Intrinsic_or_realized == in_re)
     
     data_1 <- data_raw %>%
       filter(para == para_tmp, type == type_1, Intrinsic_or_realized == in_re)
     
     data_pool_0 <- data_pool %>%
       filter(para == para_tmp, type == type_0, Intrinsic_or_realized == in_re)
     
     data_pool_1 <- data_pool %>%
       filter(para == para_tmp, type == type_1, Intrinsic_or_realized == in_re)
  }

  x <- data_0$ana_TE 
  y <- data_1$ana_TE 
  pvalue <- round(wilcox.test(x, y, alternative = "two.sided")$p.value, 3)
  
 
  mean_0 <- data_pool_0$value  
  mean_1 <- data_pool_1$value
  de_rate <- round((mean_1 - mean_0)/mean_0*100, 1)
  
  output <- data.frame(para = para_tmp, lineage_0 = type_0, lineage_1 = type_1, in_re = in_re,
                     p = pvalue, de_rate = de_rate)
  return(output)
  
}

DT1 <- pvalue("IP", "Ancestral lineage", "Alpha")
DT2 <- pvalue("IP", "Ancestral lineage", "Delta")
DT3 <- pvalue("IP", "Ancestral lineage", "BA.1")
DT4 <- pvalue("IP", "Ancestral lineage", "BA.2")
DT5 <- pvalue("IP", "Ancestral lineage", "BA.5")
DT6 <- pvalue("IP", "Alpha", "Delta")
DT7 <- pvalue("IP", "Alpha", "BA.1")
DT8 <- pvalue("IP", "Alpha", "BA.2")
DT9 <- pvalue("IP", "Alpha", "BA.5")
DT10 <- pvalue("IP", "Delta", "BA.1")
DT11 <- pvalue("IP", "Delta", "BA.2")
DT12 <- pvalue("IP", "Delta", "BA.5")
DT13 <- pvalue("IP", "BA.1", "BA.2")
DT14 <- pvalue("IP", "BA.1", "BA.5")
DT15 <- pvalue("IP", "BA.2", "BA.5")


DT16 <- pvalue("SI", "Ancestral lineage", "Alpha", "realized")
DT17 <- pvalue("SI", "Ancestral lineage", "Delta", "realized")
DT18 <- pvalue("SI", "Ancestral lineage", "BA.1", "realized")
DT19 <- pvalue("SI", "Ancestral lineage", "BA.2", "realized")
DT20 <- pvalue("SI", "Ancestral lineage", "BA.5", "realized")
DT21 <- pvalue("SI", "Alpha", "Delta", "realized")
DT22 <- pvalue("SI", "Alpha", "BA.1", "realized")
DT23 <- pvalue("SI", "Alpha", "BA.2", "realized")
DT24 <- pvalue("SI", "Alpha", "BA.5", "realized")
DT25 <- pvalue("SI", "Delta", "BA.1", "realized")
DT26 <- pvalue("SI", "Delta", "BA.2", "realized")
DT27 <- pvalue("SI", "Delta", "BA.5", "realized")
DT28 <- pvalue("SI", "BA.1", "BA.2", "realized")
DT29 <- pvalue("SI", "BA.1", "BA.5", "realized")
DT30 <- pvalue("SI", "BA.2", "BA.5", "realized")


DT31 <- pvalue("GT", "Ancestral lineage", "Alpha", "realized")
DT32 <- pvalue("GT", "Ancestral lineage", "Delta", "realized")
DT33 <- pvalue("GT", "Ancestral lineage", "BA.1", "realized")
DT34 <- pvalue("GT", "Alpha", "Delta", "realized")
DT35 <- pvalue("GT", "Alpha", "BA.1", "realized")
DT36 <- pvalue("GT", "Delta", "BA.1", "realized")

DT37 <- pvalue("GT", "Alpha", "Delta", "intrinsic")

data_res <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, 
                      DT11, DT12,DT13, DT14, DT15,DT16, DT17, DT18, DT19, DT20, 
                      DT21,DT22, DT23, DT24, DT25, DT26, DT27,DT28, DT29, DT30,
                      DT31, DT32, DT33, DT34, DT35, DT36, DT37)

data_res_1 <- data_res %>% 
  mutate(para = factor(data_res$para,levels=c("IP","SI","GT")),
         lineage_0 = factor(data_res$lineage_0,
                            levels=c("Ancestral lineage","Beta","Alpha","Delta","BA.1", "BA.2", "BA.5")),
         sign = ifelse(p <= 0.05, "*", NA)
  ) %>% 
  arrange(para, lineage_0) 
write.xlsx(data_res_1, "TEST_RESULT.xlsx")
