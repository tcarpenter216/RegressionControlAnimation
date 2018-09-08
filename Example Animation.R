#https://drsimonj.svbtle.com/visualising-residuals


library(ggplot2)
library(tidyverse)

resid <- lm((disp-mean(disp))~wt, data=mtcars)$residuals

df <- data.frame(id=1:nrow(mtcars), 
                 disp=mtcars$disp-mean(mtcars$disp), 
                 disp.wt=resid, #model with centered dv
                 disp.raw.fit=lm((disp-mean(disp))~wt, data=mtcars)$fitted, #model with centered dv
                 disp.wt.fit=lm(resid ~ mtcars$wt)$fitted,
                 wt=mtcars$wt)


# Relationship
ggplot(data=df, aes(x=wt, y=disp))+
  geom_point()+
  geom_smooth(method="lm", color="grey10")+
  geom_segment(aes(x=wt, xend=wt, y=disp, yend=disp.raw.fit), color="red")+
  theme_light()

#effect of weight removed, no relationship now
ggplot(data=df, aes(x=wt, y=disp.wt))+
  geom_point()+
  geom_smooth(method="lm", color="grey10")+
  geom_segment(aes(x=wt, xend=wt, y=disp.wt, yend=disp.wt.fit), color="red")+
  theme_light()

#make long version
names(df)

#create long version
df.long <- df %>% gather(key=kind, value=displacement, 2:3)

#do same for fitted, add to df
df.long2 <- df %>% gather(key=kind, value=fitted, 4:5)
df.long$fitted <- df.long2$fitted
rm(df.long2)

#re-arrange columns
df.long <- df.long[,c(1, 4, 6, 7, 5)]
names(df.long)

#show both separately
ggplot(data=df.long, aes(x=wt, y=displacement, fill=kind))+
  geom_point()+
  geom_point(aes(x=wt, y=fitted))+
  geom_segment(aes(x=wt, xend=wt, y=fitted, yend=displacement), color="red") +
  geom_smooth(method='lm', color="grey10")+
  theme_light()

#animated transition
library(gganimate)
ggplot(data=df.long, aes(x=wt, y=displacement))+
  geom_point()+
  geom_point(aes(x=wt, y=fitted))+
  geom_segment(aes(x=wt, xend=wt, y=fitted, yend=displacement), color="red") +
  geom_smooth(method='lm', color="grey10")+
  theme_light()+
  transition_states(kind, transition_length = 2, state_length = 1)+
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

# distance from mean
library(gganimate)
ggplot(data=df.long, aes(x=wt, y=displacement))+
  geom_point()+
  geom_point(aes(x=wt, y=fitted))+
  geom_segment(aes(x=wt, xend=wt, y=displacement, yend=mean(df.long$fitted)), color="red") +
  #geom_smooth(method='lm', color="grey10")+
  theme_light()+
  transition_states(kind, transition_length = 2, state_length = 1)+
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')



  