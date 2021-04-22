
# load the library to estimate multinomial choice models. This package needs to be installed first
library(mlogit)

# load (simulated) data about ebook readers
cbc <- read.csv("finr-final.csv",colClasses = c(Price="factor",RAM="factor",Screen.size="factor",Display.type="factor"))
cbc.mlogit <- mlogit.data(data=cbc, choice="Selected", shape="long",alt.var = 'Alt_id', id.var="ï..rep")
cbc.ml <- mlogit(Selected ~ 0 + Price + RAM + Screen_size + Display_type+RAM+Audio_jack+Internal_memory, data = cbc.mlogit)
summary(cbc.ml)

#Call:
#mlogit(formula = Selected ~ 0 + Price + RAM + Screen_size + Display_type + 
# RAM + Audio_jack + Internal_memory, data = cbc.mlogit, method = "nr")

#Frequencies of alternatives:choice
#1       2       3 
#0.26337 0.38436 0.35227 

#nr method
#4 iterations, 0h:0m:1s 
#g'(-H)^-1g = 0.000143 
#successive function values within tolerance limits 

#Coefficients :
#                      Estimate Std. Error z-value  Pr(>|z|)    
#Price19999           -0.129432   0.064955 -1.9926  0.046302 *  
#Price22999           -0.426863   0.084489 -5.0523 4.366e-07 ***
#RAM6GB                0.624444   0.078214  7.9838 1.332e-15 ***
#RAM8GB                0.740707   0.083214  8.9012 < 2.2e-16 ***
#Screen_size6"         0.138117   0.074354  1.8576  0.063231 .  
#Screen_size7"        -0.334352   0.061250 -5.4588 4.794e-08 ***
#Display_typeIPS LCD  -0.245536   0.055727 -4.4061 1.053e-05 ***
#Audio_jackYes         0.302957   0.076866  3.9414 8.102e-05 ***
#Internal_memory256GB  0.189505   0.069087  2.7430  0.006088 ** 
#Internal_memory64GB  -0.353644   0.053008 -6.6716 2.530e-11 ***
---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  #Log-Likelihood: -3068.1
  
cbc


xtabs(Selected ~ Price, data=cbc)



#Call:
mlogit(formula = Selected ~ 0 + as.numeric((Price)) + RAM + Screen_size + 
         Display_type + RAM + Audio_jack + Internal_memory, data = cbc.mlogit, 
       method = "nr")

#Frequencies of alternatives:choice
#1       2       3 
#0.26337 0.38436 0.35227 

#nr method
#4 iterations, 0h:0m:1s 
#g'(-H)^-1g = 0.000102 
#successive function values within tolerance limits 

#Coefficients :
#                      Estimate Std. Error z-value  Pr(>|z|)    
#as.numeric((Price))  -0.201235   0.041356 -4.8659 1.140e-06 ***
#RAM6GB                0.612202   0.077997  7.8490 4.219e-15 ***
#RAM8GB                0.754908   0.082795  9.1178 < 2.2e-16 ***
#Screen_size6"         0.162896   0.072413  2.2495  0.024479 *  
#Screen_size7"        -0.294861   0.054674 -5.3931 6.927e-08 ***
#Display_typeIPS LCD  -0.257740   0.055073 -4.6800 2.869e-06 ***
#Audio_jackYes         0.350307   0.069617  5.0319 4.857e-07 ***
#Internal_memory256GB  0.182324   0.068807  2.6498  0.008054 ** 
#Internal_memory64GB  -0.360083   0.053073 -6.7847 1.163e-11 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Log-Likelihood: -3069.1
#set.seed(222)

#willingness to pay for each attribute
cbc2.ml <- mlogit(Selected ~ 0 + as.numeric((Price)) + RAM + Screen_size + Display_type+RAM+Audio_jack+Internal_memory, data = cbc.mlogit)
summary(cbc2.ml)


#MWTP Calculation
coef(cbc2.ml)["RAM6GB"]/(-coef(cbc2.ml)["as.numeric((Price))"]/1000)
coef(cbc2.ml)["RAM8GB"]/(-coef(cbc2.ml)["as.numeric((Price))"]/1000)
coef(cbc2.ml)["Display_typeIPS LCD"]/(-coef(cbc2.ml)["as.numeric((Price))"]/1000)
coef(cbc2.ml)["Audio_jackYes"]/(-coef(cbc2.ml)["as.numeric((Price))"]/1000)
coef(cbc2.ml)["Internal_memory256GB"]/(-coef(cbc2.ml)["as.numeric((Price))"]/1000)
coef(cbc2.ml)["Internal_memory64GB"]/(-coef(cbc2.ml)["as.numeric((Price))"]/1000)


#Share prediction
products <- select(cbc, -c(ï..rep, Selected))
x <- predict(cbc.ml, products) # x is 2000x3 matrix
share <- t(x) # transpose x
shares <- cbind(share, products)
head(shares)





