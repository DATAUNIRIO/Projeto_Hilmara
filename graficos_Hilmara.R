##------------------------------------------------------------------------------------------------------##
##                                                                                                      ##
##    Nome: Estrategias de visualizacao de dados                                                        ##
##                                                                                                      ##
##                                                                                                      ##
##                                                                                                      ##
##    UNIRIO                                                                                            ##
##------------------------------------------------------------------------------------------------------##


# leitura de dados

library(readxl)
diretas <- read_excel("C:/Users/Steven/Documents/GitHub/Projeto_Hilmara/causas diretas cid10.xlsx", 
                                   sheet = "CAUSASDIRETAS (2)", skip = 2)

indiretas <- read_excel("C:/Users/Steven/Documents/GitHub/Projeto_Hilmara/causas diretas cid10.xlsx", 
                                   sheet = "CAUSAS INDIRETAS (2)", skip = 2)
View(diretas)
View(indiretas)

# quatro possiveis estrategias

# lolliplot
# treemap
# sunburstR
# waffle

names(diretas)

library(ggplot2)
##############################################################
#                           Lolliplot                        #  
##############################################################
#                          Causa direta                      #  
##############################################################
theme_set(theme_bw())
ggplot(diretas, aes(x=`Categoria`, y=N, label=N)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = `Categoria`, 
                   yend = N, 
                   xend = `Categoria`), 
               color = "black") +
  geom_text(color="white", size=0.6) +
  labs(title="Causas diretas") + 
  ylim(0, 22) +
  coord_flip()

# Causa indireta
theme_set(theme_bw())
ggplot(indiretas, aes(x=`Categoria`, y=N, label=N)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = `Categoria`, 
                   yend = N, 
                   xend = `Categoria`), 
               color = "black") +
  geom_text(color="white", size=0.6) +
  labs(title="Causas indiretas") + 
  ylim(0, 22) +
  coord_flip()

##############################################################
#                          Treemap                           #  
##############################################################
#                          Causa direta                      #  
##############################################################

names(diretas)

# library
library(treemap)
treemap(diretas, index=c("Grupo","Categoria"), vSize="N", type="index",
        fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","orange"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F                        # If true, labels are bigger when rectangle is bigger.
)

##############################################################
#                           sunburst                         #  
##############################################################
#                          Causa direta                      #  
##############################################################

#                     NAO ESTA FUNCIONANDO


library(sunburstR)
library(dplyr)
sunburst(diretas)

sb <- diretas %>%
  arrange(desc('Macro'), 'Categoria') %>%
  sunburst('N')
sb


##############################################################
#                           waffle                           #  
##############################################################

library(waffle)

parts <- c('Morte obstétrica de causa não especificada' = 19, 'Morte, por qualquer causa obstétrica, \nque ocorre mais de 42 dias, mas menos de 1 ano, após o parto' = 2)
p <- waffle(parts, rows = 3, colors = c("red", "blue"))
p

