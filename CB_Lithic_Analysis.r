library(tidyverse)
library(ggcorrplot)
theme_set(theme_bw())
sink('CB-analysis-output.txt')

#This is the R code to reproduce artefact analysis for the submerged lithics at Cape Bruguieres.
#Benjamin J, O’Leary M, McDonald J, Wiseman C, McCarthy J, Beckett E, et al. (2020) Aboriginal artefacts on the continental shelf reveal ancient drowned cultural landscapes in northwest Australia. PLoS ONE 15(7): e0233912. 
#https://doi.org/10.1371/journal.pone.0233912

#It requires the packages above, and several datafiles in the working directory
#- a csv file called :CB_DI_SizeSummary.csv" that includes the binned sizes of artefacts as directly measured
cb_sizes <- read_csv("CB_DI_SizeSummary.csv")
#- a csv file called "CB_DI_Artefacts_Types.csv that includes summary statistics on the type of artefacts
types <- read_csv("CB_DI_Artefacts_Types.csv")
#- a csv file called "CB_ContextCam.csv" that includes binned measurements of all artefacts, including ones estimated from underwater video footage.
ContextCam <- read_csv("CB_ContextCam.csv") #import

#Function to calculate effect sizes as Cramer's V
cramer <- function(chi_test){
  n=(sum(chi_test$observed))
  Xsq = as.numeric(chi_test$statistic)
  dfstar = (min(ncol(chi_test$observed),nrow(chi_test$observed))) - 1
  cramer_calc = sqrt(Xsq/(n*dfstar))
  return(c("data" = chi_test$data.name, "n"=n, "df*"=dfstar, "Xsq"=Xsq, "cramer" = cramer_calc))
}

#Artefact Size Distribution

#Graph of Size Distribution
cb_sizes_count <- gather(cb_sizes, Size, Count, `0-2cm`:`18-20cm`)
freq <- cb_sizes_count %>% group_by(Location, Size) %>% summarize(n=Count) %>% mutate(freq=n/sum(n)) 
ggplot(freq, aes(fill=Location, group=Location, x=Size, y=freq)) + 
  geom_bar(position="dodge", stat="identity", size=1) +
  scale_x_discrete(limits=c("0-2cm","2-4cm","4-6cm","6-8cm","8-10cm", "10-12cm", "12-14cm", "14-16cm", "16-18cm", "18-20cm")) +
  scale_fill_manual("Location", values = c("CB Channel" = "Blue", "CB Platform" = "Red", 'DI Intertidal'="skyblue", 'DI Land'='coral')) +
  ylab("Frequency") + ggtitle("Frequency of Stone Artefacts by Size")
ggsave("Fig16_Frequency_Size_CB_DI.tiff", width = 18, height = 12, units = "cm", dpi = 300)

#Chi Squared Test of the size in all 4 areas, the effect size and the residual plot
cb_sizes_chisq <- cb_sizes %>% remove_rownames %>% column_to_rownames(var="Location")
chisq <- chisq.test(cb_sizes_chisq, simulate.p.value = FALSE)
monte <- chisq.test(cb_sizes_chisq, simulate.p.value = TRUE)
print(monte)
print(cramer(chisq))

ggcorrplot(chisq$residuals, method = 'circle') + 
  ggplot2::scale_fill_gradient2(
    low = 'red',
    high = 'royalblue',
    mid = 'white',
    midpoint = 0,
    limit = c(-7, 11),
    space = "Lab",
    name = "Residual"
  ) + ggtitle("Chi-square residuals of lithic sizes") +coord_flip()
ggsave("Fig17_chi_lithic_size_CB_DI.tiff", width = 15, height = 7, units = "cm", dpi = 300)


#Chi Squared Test of the size only at CB, and the residual plot
just_cb_sizes_chisq <- cb_sizes[-3:-4,] %>% remove_rownames %>% column_to_rownames(var="Location")
justcb_chisq <- chisq.test(just_cb_sizes_chisq, simulate.p.value = FALSE)
justcb_monte <- chisq.test(just_cb_sizes_chisq, simulate.p.value = TRUE)
print(justcb_monte)
print(cramer(justcb_monte))

ggcorrplot(justcb_chisq$residuals, method = 'circle') + 
  ggplot2::scale_fill_gradient2(
    low = 'red',
    high = 'royalblue',
    mid = 'white',
    midpoint = 0,
    limit = c(-7, 11),
    space = "Lab",
    name = "Residual"
  ) + ggtitle("Chi-square residuals of lithic sizes") +coord_flip()
ggsave("Fig07_chi_lithic_size_CB.tiff", width = 15, height = 6, units = "cm", dpi = 300)


#Artefact Types at CB
#Import artefact types
row.names(types) <- types$X1
types$X1 <- NULL #remove names

#Chi-squared test and effect size
chisq.types <-chisq.test(types)
print(chisq.types)
print(cramer(chisq.types))

just_cb_types_chisq <- types[-1:-2,]

rownames(just_cb_types_chisq) <- c('CB Channel', 'CB Platform')
justcb_types_chisq <- chisq.test(just_cb_types_chisq, simulate.p.value = FALSE)
print(justcb_types_chisq)
print(cramer(justcb_types_chisq))

ggcorrplot(justcb_types_chisq$residuals, method = 'circle') + 
  ggplot2::scale_fill_gradient2(
    low = 'red',
    high = 'royalblue',
    mid = 'white',
    midpoint = 0,
    limit = c(-4, 5),
    space = "Lab",
    name = "Residual"
  ) + ggtitle("Chi-square residuals of lithic types")  + coord_flip()
ggsave("Fig08_chi_lithic_types_CB_Only.tiff", width = 17, height = 6, units = "cm", dpi = 300)


#Relationship between size and depth
model <- lm(`Size range (cm)`~`Z`, ContextCam) #linear model
print(summary(model))

#and plot
ggplot(ContextCam, aes(x =ContextCam$`Size range (cm)` , y=ContextCam$`Z`)) + 
  geom_jitter() +
  geom_smooth(method = 'lm') +
  ylab('Depth') + xlab('Size') + ggtitle('Depth vs Artefact Size') + labs(subtitle = 'Adjusted R² = 0.01102. No signficant correlation between depth and size (p=0.08074).')
ggsave("Fig12_depth_v_size.tiff", width = 17, height = 10, units = "cm", dpi = 300)

