## MXB242 Project Code Group 27  



rm(list = ls()) # Clear the enviroment.
graphics.off()  # Clears plots, closes all graphics devices
cat("\014")  # Clears console (ctrl+ L)

pacman::p_load(pacman,tidyverse, rio, knitr, glmnet, markdown, rmarkdown, gridExtra, pander, broom)

# Read in data set 
data <- read.csv('Data.csv')

# Renaming columns for ease of use 
df <- data %>%  
   as_tibble() %>%        # Convert to tibble
   select(                  # Select, reorder and rename
      Treatment = Treatment.num.,
      Dist = Distance..mm.,
      SAL = Swing.Arm.Length..mm.,
      CW = Counterweight..g.,
      PW = Projectile.weight..g.,
      treatmean = Treat.mean) 

df <- df %>%
   sort_by(df$Treatment)

df <- df %>%
   mutate(CSAL = CW*SAL,
          PWCW_ratio = (PW/CW),   # projectile to counterweight ratio
          CWPW_ratio = (CW/PW),   # counterweight to projectile ratio
          CWPW = CW*PW,
          PSAL = PW*SAL,
          ALLINT = CW*PW*SAL,
          Ex_ratio = ((2*CW)/PW)*(SAL*0.707))



### treatment order code 

## Treatments
treatments <-  c("small-short-450", "small-short-600", "small-short-750", 
                 "small-long-450", "small-long-600", "small-long-750",
                 "large-short-450", "large-short-600", "large-short-750",
                 "large-long-450", "large-long-600", "large-long-750")

# set the seed for reproducible results

set.seed(1)

# Randomise the list.
sample(treatments)


### Explorartory Analysis code

datsum <- summary(df[2:5])
kable(datsum, 
      caption = "Data Statistics Summary")

# Treatment means 
plot1 <- ggplot(df, 
                aes(x = 0, y = Dist,)) +
   geom_boxplot() +
   geom_point() +
   xlim(c(-1,1)) +
   ylab("Mean Distance (mm)") + 
   xlab("Response Variable") +
   theme(legend.position = "none")

plot2 <- ggplot(df, 
                aes(x = Dist)) + 
   geom_histogram(binwidth = 300) +
   xlab("Distance")

grid.arrange(plot1, plot2, ncol = 2)

# PW vs Dist
plot3 <- ggplot(df, 
                aes(x = PW, y = Dist, 
                    group = PW,
                    color = PW)) +
   geom_boxplot() +
   geom_point()+
   labs(y = "Distance", x = "Projectile Weight") +
   theme(legend.position = "none")
# CW vs Dist
plot4 <- ggplot(df, 
                aes(x = CW, y = Dist, 
                    group = CW,
                    color = CW)) +
   geom_boxplot() +
   geom_point()+
   labs(y = '', x = "Counterweight") +
   theme(legend.position = "none")

# SAL vs Dist  
plot5 <- ggplot(df, 
                aes(x = SAL, y = Dist, 
                    group = SAL, 
                    color = SAL)) +
   geom_boxplot() +
   geom_point()+
   labs(y = '', x = "Swing Arm Length") +
   theme(legend.position = "none")

grid.arrange(plot3, plot4, plot5, ncol = 3)

swp1 <- ggplot(df, 
               aes(CW, Dist,
                   group = SAL,
                   color = SAL)) +
   geom_point(size = 2) +
   labs(x = "Counterweight", y = "Distance") +
   lims( y = c(200,6500))+
   theme(legend.position = "none") +
   geom_smooth(method = lm)

swp2 <- ggplot(df, 
               aes(PW, Dist,
                   group = SAL,
                   color = SAL)) +
   geom_point(size = 2) +
   labs(x = "Projectile weight", y = "") +
   lims( y = c(200,6500))+
   theme(legend.position = "none") +
   geom_smooth(method = lm)

pwp1 <- ggplot(df, 
               aes(CW, Dist,
                   group = PW,
                   color = PW)) +
   geom_point(size = 2) +
   labs(x = "Counterweight", y = "Distance") +
   theme(legend.position = "none") +
   geom_smooth(method = lm)

pwp2 <- ggplot(df, 
               aes(SAL, Dist,
                   group = PW,
                   color = PW)) +
   geom_point(size = 2) +
   labs(x = "Swingarm length", y = "") +
   theme(legend.position = "none") +
   geom_smooth(method = lm)
grid.arrange(swp1, swp2, ncol = 2)



### Analysis code 


# Initial model 
fit.lm <- lm(Dist ~ CW + SAL + PW, data = df)
summary(fit.lm) %>%
   pander()

# plots
plot6 <- ggplot(df, 
                aes(PW, fit.lm$residuals,
                    color = SAL)) +
   geom_point(size = 2) +
   ylab("Residuals") + 
   xlab("Projectile Weight") + 
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')

plot7 <- ggplot(df, 
                aes(CW, fit.lm$residuals,
                    color = SAL)) +
   geom_point(size = 2) +
   ylab("Residuals") + 
   xlab("Counterweight") + 
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')

plot8 <- ggplot(df, 
                aes(SAL, fit.lm$residuals,
                    color = SAL)) +
   geom_point(size = 2) +
   ylab("Residuals") + 
   xlab("Swing Arm Length") + 
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')


plot9 <- ggplot(df, 
                aes(fit.lm$fitted.values, fit.lm$residuals,
                    color = SAL)) +
   geom_point(size = 2) +
   ylab("Residuals") + 
   xlab("Fitted Values") +
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')

indi <- ggplot(df, 
               aes(x = 1:36, y = fit.lm$residuals)) + 
   geom_line() +
   ylab("Residuals") + 
   xlab("Index") +
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')


hist <- ggplot(NULL, 
               aes(x = fit.lm$residuals)) + 
   geom_histogram(binwidth = 75) +
   xlab("Histogram")

qplot <- ggplot(NULL, 
                aes(sample = fit.lm$residuals)) + 
   geom_qq() +
   geom_qq_line() +
   xlab("QQ Plot")

grid.arrange(plot9, plot6, plot7,plot8, ncol = 2, nrow = 2)

# correlation matrix for multicollinearity
CM <- cor(df[,3:5])
cormat <- round(CM,2) 
knitr::kable(cormat, 
             caption = "Data Statistics Summary")

# expected model
expect.lm <- lm(Dist ~ (Ex_ratio) + CW + SAL + PW, data = df)
expect.out <- tidy(expect.lm)
expect.out[,2] <- round(expect.out[,2], 2)
expect.out[,3:5] <- round(expect.out[,3:5], 4)
expect.out[2,1] <- "Expected Ratio"
kable(expect.out, 
      caption = "Expected model")

# stepwise 
full.lm <- lm(Dist ~ CW + SAL + PW + CW/PW + PW*SAL + CW*SAL + PW*CW*SAL, data = df)
stepped.lm <- step(full.lm, direction = 'backward')

# table
dat <- list( 
   c("Dist = CW +SAL + PW + CW:PW + PW:SAL + CW:SAL+ CW:PW:SAL", "Dist = CW +SAL + PW + CW:PW + PW:SAL + CW:SAL", "Dist = CW +SAL + PW + CW:PW + CW:SAL"),
   c(414.65, 412.78, 410.78))
# Name repair
dat <- as_tibble(dat, 
                 .name_repair = "unique")
# Name the columns
stepdat <- dat %>% 
   rename(Model = "...1") %>% 
   rename(AIC    = "...2") 
# Display the table
knitr::kable(stepdat, 
             caption = "Stepwise Summary")


# F test
reduced.step <- lm(Dist ~ CW + PW + SAL + CW / PW, data = df)
anova(reduced.step,stepped.lm) %>%
   pander(caption = "F test")



### Final model code 


#linear model 
final.lm <- lm(formula = Dist ~ CW + PW + SAL + (CW/PW), data = df)
final.out <- tidy(final.lm)
final.out[,2] <- round(final.out[,2], 2)
final.out[,3:5] <- round(final.out[,3:5], 4)
final.out[5,1] <- "Weight Ratio"
kable(final.out, 
      caption = "Final model")

# anova 
final.aov <- aov(Dist ~ as.factor(CW)/as.factor(PW) + as.factor(CW) + as.factor(PW) + as.factor(SAL), data = df)
summary(final.aov) %>% 
   pander()

# tukeys HSD
final.tuk <- TukeyHSD(final.aov)
tuksum <- tidy(final.tuk)
tuksum[1:3,1] <- "CW"
tuksum[4,1] <- "PW"
tuksum[5,1] <- "SAL"
tuksum[6:20,1] <- "CW/PW"
kable(tuksum, 
      caption = " TukeyHSD")

# Assumptioin plots
plot6 <- ggplot(df, 
                aes(PW, final.lm$residuals,
                    color = SAL)) +
   geom_point(size = 2) +
   ylab("Residuals") + 
   xlab("Projectile Weight") + 
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')

plot7 <- ggplot(df, 
                aes(CW, final.lm$residuals,
                    color = SAL)) +
   geom_point(size = 2) +
   ylab("Residuals") + 
   xlab("Counterweight") + 
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')

plot8 <- ggplot(df, 
                aes(SAL, final.lm$residuals,
                    color = SAL)) +
   geom_point(size = 2) +
   ylab("Residuals") + 
   xlab("Swing Arm Length") + 
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')


plot9 <- ggplot(df, 
                aes(final.lm$fitted.values, final.lm$residuals,
                    color = SAL)) +
   geom_point(size = 2) +
   ylab("Residuals") + 
   xlab("Fitted Values") +
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')

indi <- ggplot(df, 
               aes(x = 1:36, y = final.lm$residuals)) + 
   geom_line() +
   ylab("Residuals") + 
   xlab("Index") +
   theme(legend.position = "none")+ 
   geom_hline(aes(yintercept = 0), colour ='red')


hist <- ggplot(NULL, 
               aes(x = final.lm$residuals)) + 
   geom_histogram(binwidth = 75) +
   xlab("Histogram")

qplot <- ggplot(NULL, 
                aes(sample = final.lm$residuals)) + 
   geom_qq() +
   geom_qq_line() +
   xlab("QQ Plot")

grid.arrange(plot9, plot6, plot7,plot8,indi, hist, qplot, ncol = 2, nrow = 4)

# unusual obs 
k <- 5 # number of predictors
n <- NROW(df) # sample size
X <- model.matrix(final.lm)


Unusual_Observations <- data.frame(leverage = hat(X),
                                   standardised = rstandard(final.lm),
                                   studentised = rstudent(final.lm),
                                   cooks = cooks.distance(final.lm))

which(Unusual_Observations$leverage>(2*(k+1)/n))

which(abs(Unusual_Observations$standardised)>2)

which(abs(Unusual_Observations$studentised)>2) # a result is called an outlier when the studentised residual is greater than 2.


which(Unusual_Observations$cooks>1) # Leverage points




