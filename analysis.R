# Preliminaries -----------------------------------------------------------

require(ggplot2)
require(dplyr)
require(psych)
require(lattice)
se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

# Import data -----------------------------------------------------------

# only works in Rstudio -- otherwise you have to set the path manually!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data = read.csv('data.csv') %>% arrange(subject) %>% filter(rating > -1) %>% mutate(rating = rating / 8)

# Compute correlations ----------------------------------------------------

# all the models
# "x" stands for Prob(green = 1); "a" stands for Prob(blue = 1)

normed = function(x,a,f) {exp(f(x,a)) / (exp(f(x,a)) + exp(f(a,x)))}
sample_model = function(x,a) {ifelse(a == 1 & x == 1, 1/2, a*(1-x)/(1-x*a))}
icard = function(x,a) {1-x*(1-a)} # model from Icard et al.
hh = function(x,a) {ifelse(x > .5, 0, ifelse(x < a, 1, 1/2))} # model from Halpern & Hitchcock
sp = function(x,a) {a*(1-x)} # model from Spellman
dp = function(x,a) {a} # delta-P model (and Power-PC model)

df.summary = data %>% group_by(green_prob, blue_prob) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))
df.cors = data.frame(x = numeric(), a = numeric(), actual = numeric(), sample = numeric(), sp = numeric(), dp = numeric(), hh = numeric(), icard = numeric(),
                     sample_normed = numeric())
df.modeling = matrix(0,10,10)
for (px in 1:10) {
  for (pa in 1:10) {
        x = px/10
        a = pa/10
        actual = df.summary$rating.mean[df.summary$green_prob == px & df.summary$blue_prob == pa]
        df.cors = rbind(df.cors, data.frame(x = px, a = pa, actual = actual, 
                                            sample = sample_model(x,a), sp = sp(x,a), dp = dp(x,a), hh = hh(x,a), icard = icard(x,a),
                                            sample_normed = normed(x,a,sample_model)))
        df.modeling[px, pa] = actual
  }
}

# check correlations b/w models and ratings
cor.test(df.cors$actual, df.cors$sample) # .86
cor.test(df.cors$actual, df.cors$sample_normed) # .87

# is our model significantly more correlated than the next-best?
r.test(n=100, r12 = cor(df.cors$actual, df.cors$sample, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$sp, use = "complete.obs"), 
       r23 = cor(df.cors$sample, df.cors$sp, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$sample_normed, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$sp, use = "complete.obs"), 
       r23 = cor(df.cors$sample_normed, df.cors$sp, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$sample, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$dp, use = "complete.obs"), 
       r23 = cor(df.cors$sample, df.cors$dp, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$sample_normed, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$dp, use = "complete.obs"), 
       r23 = cor(df.cors$sample_normed, df.cors$dp, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$sample, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$hh, use = "complete.obs"), 
       r23 = cor(df.cors$sample, df.cors$hh, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$sample_normed, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$hh, use = "complete.obs"), 
       r23 = cor(df.cors$sample_normed, df.cors$hh, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$sample, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$icard, use = "complete.obs"), 
       r23 = cor(df.cors$sample, df.cors$icard, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$sample_normed, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$icard, use = "complete.obs"), 
       r23 = cor(df.cors$sample_normed, df.cors$icard, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$sample_normed, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$sample, use = "complete.obs"), 
       r23 = cor(df.cors$sample_normed, df.cors$sample, use = "complete.obs"))


# Graph results -----------------------------------------------------------

theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_rect(colour = "black"),
             axis.text=element_text(size=20, colour = "black"), axis.title=element_text(size=24, face = "bold"),
             axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20),
             plot.title = element_text(size = 26, face = "bold", vjust = 1))

df.cors.2d = df.cors %>% mutate_at(vars(x,a),funs(factor(.,labels = paste0(seq(10,100,10),"%"))))

splot = function(aesthetic) {
  ggplot(df.cors, aesthetic) +
    geom_point(size = 3) +
    geom_smooth(method = "lm") +
    xlim(0,1) + 
    ylim(0,1) +
    labs(x = "", y = "") +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.line = element_line(colour = "black", size = 1),panel.background = element_blank())
}

threedplot = function(formula, ckey) {
  wireframe(formula, data = df.cors, colorkey = ckey, drape = TRUE,  screen=list(z=120, x=-70, y=0),
            xlab = list(""), ylab = list(""), zlab = list(""), par.settings = list(axis.line = list(col = NA)),
            scales = list(col = 1, relation = 'free', lwd = 3), zlim = c(0, 1))
}

twodplot = function(aesthetic, normed) {
  ggplot(df.cors.2d,aesthetic)+
    geom_line(size=3)+
    geom_point(size=4)+
    labs(x = '',
         color = '',
         y = '')+
    scale_color_grey(start = 0.85, end = 0)+
    theme_bw()+
    theme(text = element_text(size = 26,color='black'),
          panel.grid = element_blank(),
          legend.position = 'none',
          legend.direction = 'horizontal'
    )+
    #guides(col = guide_legend(nrow = 2, direction = 'vertical', byrow=T)) +
    ylim(ifelse(normed, .2, 0), ifelse(normed, .8, 1))+
    scale_y_continuous(breaks = c()) +
    scale_x_discrete(breaks = c())
}

# SAMPLE model, unnormed
splot(aes(x=sample,y=actual))
threedplot(sample ~ a * x, T)
twodplot(aes(x = x, y = sample, group = a, color = a), F)

# SAMPLE model, normed
splot(aes(x=sample_normed,y=actual))
threedplot(sample_normed ~ a * x, T)
twodplot(aes(x = x, y = sample_normed, group = a, color = a), T)