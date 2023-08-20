library(openxlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)


# ----FUNCTION
tmp_choose <- function(data){
  
  data$choose <- NA
  data$choose[is.na(data$mean)==F & is.na(data$mean_ci)==F] <- 1 #mean + mean_ci
  data$choose[is.na(data$mean)==F & is.na(data$mean_cri)==F & is.na(data$choose)] <- 2  #mean + mean_cri
  data$choose[is.na(data$median)==F & is.na(data$median_ci)==F & is.na(data$choose)] <- 3  #median + median_ci
  data$choose[is.na(data$median)==F & is.na(data$median_cri)==F &is.na(data$choose)] <- 4  #median + median_cri
  data$choose[is.na(data$median)==F & is.na(data$IQR)==F &is.na(data$choose)] <- 5  #median + IQR
  data$choose[is.na(data$median)==F & is.na(data$range)==F &is.na(data$choose)] <- 6  #median + RANGE
  data$choose[is.na(data$mean)==F & is.na(data$IQR)==F &is.na(data$choose)] <- 7  #mean + IQR
  data$choose[is.na(data$mean)==F & is.na(data$range)==F &is.na(data$choose)] <- 8  #mean + RANGE
  
  data$choose[is.na(data$mean)==F &is.na(data$choose)] <- 9  #mean
  data$choose[is.na(data$median)==F &is.na(data$choose)] <- 10 #median
   
  data$choose[is.na(data$IQR)==F &is.na(data$choose)] <- 11 #IQR
  data$choose[is.na(data$range)==F &is.na(data$choose)] <- 12 #RANGE
  
  data$Estimate_type <- NA
  data$Uncertainty_type <- NA
  data$point <- NA
  data$interval <- NA
  
 
  for(i in 1:nrow(data)){
    #point
    if(data[i,]$choose %in% c(1, 2, 7, 8, 9)){
      data[i,]$Estimate_type <- "Mean"
      data[i,]$point <- data[i,]$mean
    }else{
      data[i,]$Estimate_type <- "Median"
      data[i,]$point <- data[i,]$median
    }
  
  #interval
  if(data[i,]$choose %in% c(1)){
    data[i,]$Uncertainty_type <- "95%CI"
    data[i,]$interval <- data[i,]$mean_ci
  }
  if(data[i,]$choose %in% c(2)){
    data[i,]$Uncertainty_type <- "95%CrI"
    data[i,]$interval <- data[i,]$mean_cri
  }
  if(data[i,]$choose %in% c(3)){
    data[i,]$Uncertainty_type <- "95%CI"
    data[i,]$interval <- data[i,]$median_ci
  }
  if(data[i,]$choose %in% c(4)){
    data[i,]$Uncertainty_type <- "95%CrI"
    data[i,]$interval <- data[i,]$median_cri
  }
  if(data[i,]$choose %in% c(5,7,11)){
    data[i,]$Uncertainty_type <- "IQR"
    data[i,]$interval <- data[i,]$IQR
  }
  if(data[i,]$choose %in% c(6,8,12)){
    data[i,]$Uncertainty_type <- "Range"
    data[i,]$interval <- data[i,]$range
  }
  
  }
  return(data)
}

tmp_split_1 <- function(data_w){
  
  data_w$low <- NA
  data_w$up <- NA
  
  
  for(i in 1:nrow(data_w)){
    if(is.na(data_w[i,]$interval)==F){
      
      string <- data_w[i,]$interval
      x <- gregexpr("-",string)[[1]]
      loc <- x[length(x)]
      n <- nchar(string)
      s1 <- substr(string, 1, loc-1)
      s2 <- substr(string, loc+1, n)
      
      data_w[i,]$low = s1
      data_w[i,]$up = s2
      
    }
  }
  return(data_w)
}

#------
data_raw <- read_excel("code/public/DATA_RAW.xlsx") %>%
  mutate(type = ifelse(!is.na(sublineage), sublineage, type))
  

data_plot <- data_raw %>% tmp_choose(.) %>% tmp_split_1(.) 
data_plot$type <- factor(data_plot$type, 
                         levels = c("Ancestral lineage", "Alpha","Beta", "Delta", 
                                    "BA.1", "BA.2","BA.4","BA.5","unspecified"))
data_plot$point <- as.numeric(data_plot$point)
data_plot$low <- as.numeric(data_plot$low)
data_plot$up <- as.numeric(data_plot$up)


data_plot <- data_plot %>% filter(!(type == "Ancestral lineage"))
#--------IP -- FIG 3 ---------------------
data_tmp <- data_plot %>%
  filter(para == "IP") %>%
  arrange(type, mid_date) %>%
  mutate(loc = rev(c(1:2, 5:7, 10:12, 15:25, 28:47, 50, 53:54)))

fig3 <- ggplot(data_tmp, aes(point, loc)) +
  annotate("rect", xmin = 5.88, xmax = 7.12, ymin = 0,    ymax = 55, alpha = 0.15, fill = "#01545a") + #ancestral
  annotate("rect", xmin = 4.53, xmax = 5.3,  ymin = 51.5, ymax = 55, alpha = 0.2, fill = "#a73323") +  #alpha
  annotate("rect", xmin = 4.11, xmax = 5.15, ymin = 26.5, ymax = 48.5, alpha = 0.2, fill = "#a73323") + #delta
  annotate("rect", xmin = 3.13, xmax = 3.86, ymin = 13.5, ymax = 26.5, alpha = 0.2, fill = "#a73323") + #ba.1
  annotate("rect", xmin = 3.18, xmax = 4.93, ymin = 8.5,  ymax = 13.5, alpha = 0.2, fill = "#a73323") + #ba.2
  annotate("rect", xmin = 2.01, xmax = 5.61, ymin = 3.5,  ymax = 8.5, alpha = 0.2, fill = "#a73323") + #ba.5
  annotate("rect", xmin = 2.98, xmax = 3.59, ymin = 0,  ymax = 3.5, alpha = 0.2, fill = "#a73323") + #unspecified
  
  geom_segment(x = 6.50, xend = 6.50, y = 55,  yend = 0,     lty = 2, size = 1, color = "#55b5bc") +  #an
  geom_segment(x = 4.92, xend = 4.92, y = 55, yend = 51.5,   lty = 2, size = 1, color = "#dd7f72") + #alpha
  geom_segment(x = 4.63, xend = 4.63, y = 48.5, yend = 26.5, lty = 2, size = 1, color = "#dd7f72") + #delta
  geom_segment(x = 3.49, xend = 3.49, y = 26.5, yend = 13.5, lty = 2, size = 1, color = "#dd7f72") + #ba.1
  geom_segment(x = 4.06, xend = 4.06, y = 13.5, yend = 8.5,  lty = 2, size = 1, color = "#dd7f72") + #ba.2
  geom_segment(x = 3.81, xend = 3.81, y = 8.5, yend = 3.5,   lty = 2, size = 1, color = "#dd7f72") + #ba.5
  geom_segment(x = 3.29, xend = 3.29, y = 3.5, yend = 0,     lty = 2, size = 1, color = "#dd7f72") + #unspecified
  
  geom_errorbarh(aes(xmax = up, xmin = low, color = Uncertainty_type), size = 1) +
  scale_color_manual(
    values= c("95%CI"="#87ae49", "95%CrI"="#3fa294", "IQR"="#efc00c", "Range"="#f68b1f")
  )+
  guides(color=guide_legend(title = "Dispersion"))+
  
  geom_point(data = data_tmp, aes(shape = Estimate_type), size = 2)+
  labs(shape="Central tendency")+
  annotate("text", x = 8.7, y = 54, label = "Alpha") +
  annotate("text", x = 8.7, y = 50, label = "Beta") +
  annotate("text", x = 8.7, y = 47, label = "Delta") +
  annotate("text", x = 8.5, y = 25, label = "Omicron BA.1") +
  annotate("text", x = 8.5, y = 12, label = "Omicron BA.2") +
  annotate("text", x = 8.5, y = 7, label = "Omicron BA.5") +
  annotate("text", x = 8.5, y = 2, label = "Omicron unspecified") +
  
  scale_y_continuous(expand = c(0,0), limits = c(0,55),
                     breaks = data_tmp$loc,labels = data_tmp$Author_year)+
  scale_x_continuous(limits= c(0,9.5),breaks = seq(0,9.5,2),sec.axis = sec_axis(~.,breaks = seq(0,9.5,2)))+
  xlab('Incubation period')+ 
  ylab(' ')+
  geom_hline(aes(yintercept = 3.5))+
  geom_hline(aes(yintercept = 8.5))+
  geom_hline(aes(yintercept = 13.5))+
  geom_hline(aes(yintercept = 26.5))+
  geom_hline(aes(yintercept = 48.5))+
  geom_hline(aes(yintercept = 51.5))+
  theme_few()+
  theme(axis.text.x = element_text(size = 10))

fig3

tiff("Fig3.tiff",
     width = 9, height = 9, res = 326,
     units = "in", compression = "lzw")
print(fig3)
dev.off() 

#---SI -- FIG 4 ----

data_tmp <- data_plot %>%
  filter(para == "SI") %>%
  arrange(type, mid_date) %>%
  mutate(loc = rev(c(1:5, 8:9, 12, 15:22, 25:41, 44:72, 75:78)))

fig4 <- ggplot(data_tmp, aes(point, loc))+
  annotate("rect", xmin = 4.5,  xmax = 5.14, ymin = 0,    ymax = 79,   alpha = 0.15, fill = "#01545a") + #ancestral
  annotate("rect", xmin = 2.52, xmax = 4.41, ymin = 73.5 , ymax = 79,   alpha = 0.2, fill = "#a73323") + #alpha
  annotate("rect", xmin = 3.26, xmax = 3.92, ymin = 42.5, ymax = 73.5, alpha = 0.2, fill = "#a73323") + #delta
  annotate("rect", xmin = 2.94, xmax = 3.48, ymin = 23.5,    ymax = 42.5, alpha = 0.2, fill = "#a73323") + #ba.1
  annotate("rect", xmin = 2.92, xmax = 3.68, ymin = 13.5,    ymax = 23.5, alpha = 0.2, fill = "#a73323") + #ba.2
  annotate("rect", xmin = 1.71, xmax = 3.04, ymin = 6.5,    ymax = 10.5, alpha = 0.2, fill = "#a73323") + #ba.5
  annotate("rect", xmin = 2.93, xmax = 4.49, ymin = 0,    ymax = 6.5, alpha = 0.2, fill = "#a73323") + #unspecified

  geom_segment(x = 4.82, xend = 4.82, y = 0,   yend = 79,   lty = 2, size = 1 ,color = "#55b5bc") + #ancestral
  geom_segment(x = 3.47, xend = 3.47, y = 73.5, yend = 79,  lty = 2, size = 1, color = "#dd7f72") + #alpha
  geom_segment(x = 3.59, xend = 3.59, y = 42.5, yend = 73.5, lty = 2, size = 1, color = "#dd7f72") + #delta
  geom_segment(x = 3.21, xend = 3.21, y = 23.5, yend = 42.5,  lty = 2, size = 1, color = "#dd7f72") + #ba.1
  geom_segment(x = 3.3, xend = 3.3, y = 13.5, yend = 23.5,  lty = 2, size = 1, color = "#dd7f72") + #ba.2
  geom_segment(x = 2.37, xend = 2.37, y = 6.5,  yend = 10.5,  lty = 2, size = 1, color = "#dd7f72") + #ba.5
  geom_segment(x = 3.71, xend = 3.71, y = 0,    yend = 6.5,   lty = 2, size = 1, color = "#dd7f72") + #unspecified
  geom_errorbar(aes(xmax = up, xmin = low, color= Uncertainty_type), size= 1) +
  
  scale_color_manual(
    values = c("95%CI"="#87ae49", "95%CrI"="#3fa294", "IQR"="#efc00c"))+
  guides(color=guide_legend(title = "Dispersion") )+

  geom_point(data = data_tmp, aes(shape = Estimate_type), size = 2) +
  labs(shape = "Central tendency")+
  annotate("text", x = 11.5, y = 78,    label = "Alpha") +
  annotate("text", x = 11.5, y = 72, label = "Delta") +
  annotate("text", x = 11.2, y = 41,  label = "Omicron BA.1") +
  annotate("text", x = 11.2, y = 22,  label = "Omicron BA.2") +
  annotate("text", x = 11.2, y = 12,  label = "Omicron BA.4") +
  annotate("text", x = 11.2, y = 9,  label = "Omicron BA.5") +
  annotate("text", x = 11.2, y = 5,  label = "Omicron unspecified") +
  
  
  scale_y_continuous(expand=c(0,0),limits = c(0,79) , breaks = data_tmp$loc, labels = data_tmp$Author_year) +
  scale_x_continuous(limits= c(0,12.6), breaks = seq(0,12.6,2), 
                     sec.axis = sec_axis(~.,breaks = seq(0,12.6,2))) +
  xlab('Serial interval')+ 
  ylab(' ') +
  geom_hline(aes(yintercept = 73.5)) +
  geom_hline(aes(yintercept = 42.5)) +
  geom_hline(aes(yintercept = 23.5)) +
  geom_hline(aes(yintercept = 13.5)) +
  geom_hline(aes(yintercept = 10.5)) +
  geom_hline(aes(yintercept = 6.5)) +
  theme_few() +
  theme(axis.text.x = element_text(size = 10, color = "black"))

fig4
tiff("Fig4.tiff",
     width = 9, height = 10.5, res = 326,
     units = "in", compression = "lzw")
print(fig4)
dev.off() 

#-----realized GT FIG 5----

data_tmp <- data_plot %>%
  filter(para == "GT", Intrinsic_or_realized == "realized") %>%
  arrange(type, mid_date) %>%
  mutate(loc = rev(c(1, 4:7, 10:17, 20:22)))

fig5 <- ggplot(data_tmp, aes(point, loc))+
  annotate("rect", xmin = 4.30, xmax = 5.61, ymin = 0,    ymax = 23,   alpha = 0.15, fill = "#01545a") + #ancestral
  annotate("rect", xmin = 3.91, xmax = 4.8,  ymin = 18.5, ymax = 23,   alpha = 0.2, fill = "#a73323") + #alpha
  annotate("rect", xmin = 3.25, xmax = 4.05, ymin = 8.5,  ymax = 18.5, alpha = 0.2, fill = "#a73323") + #delta
  annotate("rect", xmin = 2.48, xmax = 3.49, ymin = 2.5,    ymax = 8.5,  alpha = 0.2, fill = "#a73323") + #ba.1

  geom_segment(x = 4.95, xend = 4.95, y = 0,    yend = 23,    lty = 2, size = 1, color = "#55b5bc") + #ancestral
  geom_segment(x = 4.35, xend = 4.35, y = 18.5, yend = 23,  lty = 2, size = 1, color = "#dd7f72") + #alpha
  geom_segment(x = 3.65, xend = 3.65, y = 8.5,  yend = 18.5,  lty = 2, size = 1, color = "#dd7f72") + #delta
  geom_segment(x = 2.99, xend = 2.99, y = 2.5,  yend = 8.5,    lty = 2, size = 1, color = "#dd7f72") + #ba.1
  
  geom_errorbar(aes(xmax = up, xmin = low, color = Uncertainty_type), size = 1) +
  geom_point(data = data_tmp, aes(shape = Estimate_type), size = 2) +
  
  scale_color_manual(
    values= c("95%CI"="#87ae49", "95%CrI"="#3fa294")) +
  
  guides(shape=guide_legend(title = "Central tendency",order = 2)) +
  guides(color=guide_legend(title = "Dispersion", order = 1)) +

  annotate("text", x = 6.5, y = 22, label = "Alpha") +
  annotate("text", x = 6.5, y = 17, label = "Delta") +
  annotate("text", x = 6.4, y = 7, label = "Omicron BA.1") +
  annotate("text", x = 6.4, y = 1, label = "Omicron BA.5") +
  
  scale_y_continuous(expand = c(0,0), limits = c(0,23), breaks = data_tmp$loc, labels = data_tmp$Author_year) +
  scale_x_continuous(expand = c(0,0), limits =  c(1,7),
                     breaks = seq(1,7,2), sec.axis = sec_axis(~.,breaks = seq(1,7,2))) +
  xlab('Realized generation time') + 
  ylab(' ') +
  theme_few() +

  geom_hline(aes(yintercept = 18.5)) +
  geom_hline(aes(yintercept = 8.5)) +
  geom_hline(aes(yintercept = 2.5)) +
  theme(axis.text.x = element_text(size = 10))

fig5

tiff("Fig5.tiff",
     width = 9, height = 3.7, res = 326,
     units = "in", compression = "lzw")
print(fig5)
dev.off() 

#-----intrinsic GT FIG 6----

data_tmp <- data_plot %>%
  filter(para == "GT", Intrinsic_or_realized == "intrinsic") %>%
  arrange(type, mid_date) %>%
  mutate(loc = rev(c(1, 4:5, 8:9)))

fig6 <- ggplot(data_tmp, aes(point, loc)) +
  annotate("rect", xmin = 5.47, xmax = 6.26, ymin = 6.5, ymax = 10,  alpha = 0.15, fill = "#a73323") +
  annotate("rect", xmin = 3.79, xmax = 7.55, ymin = 2.5, ymax = 6.5, alpha = 0.2, fill = "#a73323") +
  
  geom_segment(x = 5.86, xend = 5.86, y = 10,  yend = 6.5, lty = 2, size = 1, color = "#dd7f72") + 
  geom_segment(x = 5.67, xend =5.67,  y = 6.5, yend =2.5,  lty = 2, size = 1, color = "#dd7f72") + 
  
  geom_errorbar(aes(xmax = up, xmin = low, color = Uncertainty_type), size = 1) +
  scale_color_manual(values = c("95%CrI"="#3fa294")) +
  labs(shape = "Central tendency")+
  guides(color = guide_legend(title = "Dispersion")) +
  guides(shape = guide_legend(title = "Central tendency",order = 1)) +
  guides(color = guide_legend(title = "Dispersion",order = 2)) +

  geom_point(data = data_tmp, aes(shape = Estimate_type), size = 2) +
  annotate("text", x = 9.3, y = 9, label = "Alpha") +
  annotate("text", x = 9.3, y = 5, label = "Delta") +
  annotate("text", x = 9.3, y = 1, label = "Omicron BA.1") +
  
  scale_y_continuous(expand = c(0,0), limits = c(0,10), breaks = data_tmp$loc,labels = data_tmp$Author_year) +
  scale_x_continuous(expand = c(0,0), limits = c(2,10),
                     breaks = seq(2,10,2), sec.axis = sec_axis(~.,breaks = seq(2,10,2))) +
  xlab('Intrinsic generation time')+ 
  ylab(' ') +
  theme_few() +
  geom_hline(aes(yintercept = 6.5)) +
  geom_hline(aes(yintercept = 2.5)) +
  theme(axis.text.x = element_text(size = 10, color = "black"))

fig6
tiff("Fig6.tiff",
     width = 9, height = 2.6, res = 326,
     units = "in", compression = "lzw")
print(fig6)
dev.off() 




