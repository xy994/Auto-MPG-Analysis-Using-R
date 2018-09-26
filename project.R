car=read.table("C:/Users/yangx/Documents/MA598_2018/project/car_nohead.txt")
names(car)=c("mpg","cylinders","displacement","horsepower","weight","acceleration","modelyear","origin")
head(car)
dim(car)

attach(car)

m_group = table(modelyear)
m_group
chisq.test(m_group)

c_group = table(cylinders)
c_group
chisq.test(c_group)

o_group = table(origin)
o_group
chisq.test(o_group)

group1 = mpg[modelyear=="70"]
group2 = mpg[modelyear=="71"]
group3 = mpg[modelyear=="72"]
group4 = mpg[modelyear=="73"]
group5 = mpg[modelyear=="74"]
group6 = mpg[modelyear=="75"]

group7 = mpg[modelyear=="76"]
group8 = mpg[modelyear=="77"]
group9 = mpg[modelyear=="78"]
group10 = mpg[modelyear=="79"]
group11 = mpg[modelyear=="80"]
group12 = mpg[modelyear=="81"]
group13 = mpg[modelyear=="82"]

treatment=c(rep(70,length(group1)),rep(71,length(group2)),rep(72,length(group3)),
rep(73,length(group4)),rep(74,length(group5)),rep(75,length(group6)),rep(76,length(group7)),rep(77,length(group8)),rep(78,length(group9)),
rep(79,length(group10)),rep(80,length(group11)),rep(81,length(group12)),rep(82,length(group13)) )
treatmentfactor=factor(treatment)
y=c(group1, group2, group3,group4, group5, group6,group7, group8, group9,group10, group11, group12,group13)

g=lm(y~treatmentfactor)

newcar=data.frame(mpg,displacement,horsepower,weight,acceleration,modelyear)
library(leaps)
sub = regsubsets(mpg~displacement+horsepower+weight+acceleration+modelyear,data=newcar)

x1=displacement
x2=as.numeric(horsepower)
x3=weight
x4=acceleration
x5=modelyear
y=mpg
model1=lm(y~x1+x2+x3+x4+x5)

newcar=read.delim("C:/Users/yangx/Documents/MA598_2018/project/car_nohead.txt")




