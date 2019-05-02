### R code from vignette source 'vignette-lslx.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: preliminaries
###################################################
library(lslx)
library(ggplot2)
library(knitr)
options(prompt = "R> ", continue = "+  ", width = 80, useFancyQuotes = FALSE)
options(digits = 3L)


###################################################
### code chunk number 2: vignette-lslx.Rnw:353-355
###################################################
model_reg <- "y <= x1 + x2
              y <~ x3 + x4"


###################################################
### code chunk number 3: vignette-lslx.Rnw:368-370
###################################################
model_reg <- "x1 + x2 => y
              x3 + x4 ~> y"


###################################################
### code chunk number 4: vignette-lslx.Rnw:373-374
###################################################
model_reg <- "y <= x1 + x2 + pen() * x3 + pen() * x4"


###################################################
### code chunk number 5: vignette-lslx.Rnw:377-378
###################################################
model_reg <- "y <~ free() * x1 + free() * x2 + x3 + x4"


###################################################
### code chunk number 6: vignette-lslx.Rnw:468-477
###################################################
model_fa <- "visual  :=> x1 + x2 + x3
             textual :=> x4 + x5 + x6
             speed   :=> x7 + x8 + x9
             visual  :~> x4 + x5 + x6 + x7 + x8 + x9
             textual :~> x1 + x2 + x3 + x7 + x8 + x9
             speed   :~> x1 + x2 + x3 + x4 + x5 + x6
             visual  <=> fix(1) * visual
             textual <=> fix(1) * textual
             speed   <=> fix(1) * speed"


###################################################
### code chunk number 7: vignette-lslx.Rnw:482-491
###################################################
model_fa_lavaan <- "visual  =~ x1 + x2 + x3
                    textual =~ x4 + x5 + x6
                    speed   =~ x7 + x8 + x9
                    pen() * visual  =~ x4 + x5 + x6 + x7 + x8 + x9
                    pen() * textual =~ x1 + x2 + x3 + x7 + x8 + x9
                    pen() * speed   =~ x1 + x2 + x3 + x4 + x5 + x6
                    visual  ~~ 1 * visual
                    textual ~~ 1 * textual
                    speed   ~~ 1 * speed"


###################################################
### code chunk number 8: vignette-lslx.Rnw:498-500
###################################################
lslx_fa <- lslx$new(model = model_fa, 
  data = lavaan::HolzingerSwineford1939)


###################################################
### code chunk number 9: vignette-lslx.Rnw:505-507
###################################################
lslx_fa$fit(penalty_method = "mcp",
  lambda_grid = seq(.01, .60, .01), delta_grid = c(1.5, 3.0, Inf))


###################################################
### code chunk number 10: vignette-lslx.Rnw:512-513
###################################################
lslx_fa$summarize(selector = "bic", interval = FALSE)


###################################################
### code chunk number 11: vignette-lslx.Rnw:603-604
###################################################
lslx_fa$plot_numerical_condition()


###################################################
### code chunk number 12: vignette-lslx.Rnw:611-612 (eval = FALSE)
###################################################
## lslx_fa$plot_numerical_condition()


###################################################
### code chunk number 13: vignette-lslx.Rnw:616-617 (eval = FALSE)
###################################################
## lslx_fa$plot_coefficient(block = "y<-f")


###################################################
### code chunk number 14: vignette-lslx.Rnw:623-624
###################################################
lslx_fa$plot_coefficient(block = "y<-f")


###################################################
### code chunk number 15: vignette-lslx.Rnw:633-636
###################################################
moment_jacobian <- lslx_fa$extract_moment_jacobian(
  selector = "bic", type = "effective")
min(svd(moment_jacobian)$d)


###################################################
### code chunk number 16: vignette-lslx.Rnw:673-681
###################################################
data_miss <- lavaan::HolzingerSwineford1939
data_miss$x5 <- ifelse(
  test = data_miss$x1 <= quantile(data_miss$x1, .3), 
  yes = NA, no = data_miss$x5)
data_miss$age <- data_miss$ageyr + data_miss$agemo / 12
data_miss$x9 <- ifelse(
  test = data_miss$age <= quantile(data_miss$age, .3), 
  yes = NA, no = data_miss$x9)


###################################################
### code chunk number 17: vignette-lslx.Rnw:684-692
###################################################
model_miss <- "x1 + x2 + x3 <=: visual
               x4 + x5 + x6 <=: textual
               x7 + x8 + x9 <=: speed
               visual  <=> 1 * visual
               textual <=> 1 * textual
               speed   <=> 1 * speed"
lslx_miss <- lslx$new(model = model_miss, data = data_miss,
  auxiliary_variable = c("ageyr", "agemo"), verbose = FALSE)


###################################################
### code chunk number 18: vignette-lslx.Rnw:695-696
###################################################
lslx_miss$penalize_block(block = "y<->y", type = "fixed", verbose = FALSE)


###################################################
### code chunk number 19: vignette-lslx.Rnw:699-700
###################################################
lslx_miss$fit_lasso(verbose = FALSE)


###################################################
### code chunk number 20: vignette-lslx.Rnw:703-704
###################################################
lslx_miss$summarize(selector = "raic", style = "minimal")


###################################################
### code chunk number 21: vignette-lslx.Rnw:707-708
###################################################
lslx_miss$extract_coefficient_matrix(selector = "raic", block = "y<->y")


###################################################
### code chunk number 22: vignette-lslx.Rnw:711-712
###################################################
lslx_miss$extract_fit_index(selector = "raic")


###################################################
### code chunk number 23: vignette-lslx.Rnw:750-756
###################################################
model_mgfa <- "1 * x1 + x2 + x3 <=: visual 
               1 * x4 + x5 + x6 <=: textual
               1 * x7 + x8 + x9 <=: speed"
lslx_mgfa <- lslx$new(model = model_mgfa,
  data = lavaan::HolzingerSwineford1939, group_variable = "school",
  reference_group = "Pasteur", verbose = FALSE)


###################################################
### code chunk number 24: vignette-lslx.Rnw:761-764
###################################################
model_mgfa <- "c(fix(0), fix(1)) * x1 + x2 + x3 <=: visual 
               c(fix(0), fix(1)) * x4 + x5 + x6 <=: textual
               c(fix(0), fix(1)) * x7 + x8 + x9 <=: speed"


###################################################
### code chunk number 25: vignette-lslx.Rnw:769-771
###################################################
lslx_mgfa$penalize_heterogeneity(block = c("y<-f", "y<-1"), 
  group = "Grant-White", verbose = FALSE)


###################################################
### code chunk number 26: vignette-lslx.Rnw:774-776
###################################################
lslx_mgfa$free_block(block = "f<-1", 
  group = "Grant-White", verbose = FALSE)


###################################################
### code chunk number 27: vignette-lslx.Rnw:779-780
###################################################
lslx_mgfa$fit_mcp(verbose = FALSE)


###################################################
### code chunk number 28: vignette-lslx.Rnw:783-789
###################################################
loading <- lslx_mgfa$extract_coefficient_matrix(
  selector = "hbic", block = "y<-f")
intercept <- lslx_mgfa$extract_coefficient_matrix(
  selector = "hbic", block = "y<-1")
loading$"Grant-White" - loading$"Pasteur"
t(intercept$"Grant-White" - intercept$"Pasteur")


