
library(MuMIn)
library(fmsb)

# This code serve to plot the relative variable importance (sensu Burnham & Anderson, 2002) of
# co-variates. Note that this script serve to plot relative variable importance when all covariates
# appears equally in model selection. If some covariates appear in more models than others, divide the 
# sum of the Akaike weights of the best selected models in which the variable of interest
# is present by the total number of models in which this variable appears.

#Using the dataset mtcars, we built a full model (mpg ~ cyl + disp + hp). The function dredge will return
# all possible combinations of covariates in several model. Finally we carried out a model selection

options(na.action = "na.fail") 
Full_model <- lm(mpg ~ cyl + disp + hp, data=mtcars)
Allmodels <- dredge(Full_model) 
Model_selection <- model.sel(Allmodels)

## Now we will build a data.frame summing the weight of all selected models in which each variable appears.
# To use the fmsb package, We have to add 2 lines to the dataframe: the max and min of each topic to show on the plot

cyl <- c(0.65,0,c(0.337+0.156+0.144))
disp <- c(0.65,0,c(0.337+0.161))
hp <- c(0.65,0,0.161)

importance <- data.frame(cbind(cyl,disp,hp))

#Chart code
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0,0,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0,0,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( importance  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.16,0.04), calcex=0.7,  cglwd=0.9,
            #custom labels
            vlcex=0.8 
)

