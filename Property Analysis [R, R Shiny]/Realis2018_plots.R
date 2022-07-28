packages = c('treemap', 'tidyverse','treemap','sf','tmap','readxl','knitr','ggplot2','scales','stringr')

for(p in packages){library
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

setwd("C:/Users/User/Desktop/MITB/ASAR/Realis2018")
getwd()
realis2018 <- read.csv("realis2018.csv")

head(realis2018)

colnames <- c("Project.Name","Address","No.of.Units","Area.(sqm)","Type.Of.Area","Transacted.Price","Unit.Price.psm","Unit.Pricepsf","Sale.Date","Property.Type","Tenure","Type.Of.Sale","Purchaser.Address.Indicator","Postal.District","Postal.Sector","Postal.Code","Planning.Region","Planning.Area")

colnames(realis2018) <- colnames

head(realis2018)

## --------------------------------

treemapdata <- realis2018 %>%
  group_by(`Planning.Area`,Planning.Region) %>%
  summarize('Total.Transacted.Price' = sum(`Transacted.Price`, na.rm= TRUE),
            `Total.Unit.Sold` = sum(`No.of.Units`, na.rm = TRUE),
            `Median.PSF` = median(`Unit.Pricepsf`, na.rm = TRUE)
  )

treemapdata

treemap(treemapdata,
        index = c("Planning.Region","Planning.Area"),
        vSize = "Total.Unit.Sold",
        vColor = "Median.PSF",
        type = "value",
        title = "No. of units transacted by Planning Region and Area",
        title.legend = "Median price per square foot") + scale_fill_brewer(palette = "Dark2") +                   scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

## --------------------------------



removeEnbloc <- filter (realis2018, str_detect(Address, "ENBLOC", negate = TRUE))
Tenure_Type <- ifelse(removeEnbloc$Tenure == "Freehold", "Freehold",
                      ifelse(substr(removeEnbloc$Tenure, 0, 3) == '999', 'Freehold', 'Leasehold'))

head(removeEnbloc)

Tenure_Box <-  ggplot(removeEnbloc, aes(x=Planning.Region, y= Unit.Pricepsf, 
                                        weights(No.of.Units))) + 
                        geom_boxplot(aes(color = Tenure_Type)) +   
                        facet_wrap(~Property.Type) +   
                        xlab("Tenure") +           
                        ylab("Unit Price Per Square Foot")
                      
                      Tenure_Box + theme(legend.position="bottom", 
                                          legend.title=element_text(face = "bold"), 
                                          axis.text=element_text(angle=90, size=9)) 
                      
## -----------------------------------------
                      
realis_Data2 <- realis2018[-grep("ENBLOC",realis2018$Address),]
TenureType <- ifelse(realis_Data2$Tenure == "Freehold", "Freehold",
                                           ifelse(substr(realis2018$Tenure,0,3) == "999","Freehold","Leasehold"))

realis_Data2 <- cbind(realis_Data2, TenureType)
head(realis_Data2)                      

realis_Data2 %>%
  ggplot(aes(Planning.Region, No.of.Units, fill= TenureType)) +
  geom_col(position = 'stack') +
  xlab("Planning Region") +
  ylab("Total Number of Transactions") +
  facet_grid(~Purchaser.Address.Indicator) +
  labs(title = 'No. of Transactions by Planning Region and Purchaser Address')+
  theme(legend.position = "bottom", 
        legend.title = element_text(face = "bold"), 
        axis.text.x = element_text(size = 8))

