library(mosaic)
library(dplyr)
library(mosaic)
library(knitr)
library(leaps)

#1.a. 
exam <- 100
project <- 30
final <- 11 + .53*exam + 1.2*project
final
#We would predict that they would get a perfect score on the final. 
#b.
Predicted <- 11 + 0.53*87 + 1.2*21
Predicted
residual <- 80 - Predicted
residual
#c.
#we cannot tell from just the coefficients alone what the R-squared value might be. We can
#certainly say that an amount of the final grade might be explained by how well you did on the project.
#But the amount explained might vary from model to model if say you were to add homework grades, the best
#way to find out what kind of relationship it is, is by looking at the R-squared value which is not
#directly available from the regression equation. 
#d. The coefficient is subject to change if you add or remove an explanatory variable. The coefficient
# of 1.2 suggest that for every point someone gets on the project their final grade will be 1.2 higher than
#it would have been, on average, if they had not gotten the point.

#2.a.
#The adjusted R-squared will always be less than R^2. This is because the 'k' in the formula for
#adjusted R^2 must always be greater than 0 and so numerator by divisor must always be less more than
#or equal to R^2 which makes the adjusted R-Squared always less than or equal to R-Squared.
#b.False, if the explanatory variable accounts for more variability than it would by chance than the
# R^2 will go up and so will the adjusted R^2.
#True, I'm thinking of a random distribution of points that is the new explanatory variable that doesn't
#really explain anything in the dependent variable. If this variable was added to the model then
#it would most likely have a really low coefficient, and therefore a negligible impact on the residuals,
#so R^2 would not chnage. 



