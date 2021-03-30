# 2019 代码 -----------------------------------------------------------------

df <- data.frame(
  x=c('a','b','c'),
  y=1:3,
  z=c(2,5,3)
)
df

for(i in 1:5){
  j <- i*2+10
  print(j)
}

write.csv(df,file = '我的文件.csv')


df <- data.frame(
  name=c('张三','李四','王五','赵六'),
  code=1:4,
  score=c(80,57,66,78),
  level=c('B','C','C','B')
)
df
write.csv(df,file = 'df.csv')
WriteXLS(df,'df.xls')


mydf <- data.frame(x=c('A','B','C'),
                   '2010'=c(1,3,4),
                   '2011'=c(3,5,2),
                   check.names = FALSE)
mydf
mydf2 <- gather(mydf,key='year',value='value',-x)
mydf2

regiondata2 <- gather(regiondata,
                      key='year',
                      value='GDP',
                      -region)
View(regiondata2)

regiondata3 <- spread(regiondata2,
                      key = 'year',
                      value = 'GDP')

View(regiondata3)


xing <- "赵钱孙李周吴郑王冯陈褚卫蒋沈韩杨朱秦尤许何吕施张孔曹严华金魏陶姜戚谢邹喻柏水窦章云苏潘葛奚范彭郎鲁韦昌马苗凤花方俞任袁柳酆鲍史唐费廉岑薛雷贺倪汤滕殷罗毕郝邬安常乐于时傅皮卞齐康伍余元卜顾孟平黄和穆萧尹姚邵湛汪祁毛禹狄米贝明臧计伏成戴谈宋茅庞熊纪舒屈项祝董梁杜阮蓝闵席季麻强贾路娄危江童颜郭梅盛林刁钟徐邱骆高夏蔡田樊胡凌霍虞万支柯昝管卢莫房裘缪干解应宗丁宣贲邓郁单杭洪包诸左石崔吉钮龚程嵇邢滑裴陆荣"
k <- sample(1:str_count(xing),1)
substr(xing,k,k)

malename <- "澄邈德泽海超海阳海荣海逸海昌瀚钰瀚文涵亮涵煦明宇涵衍浩皛浩波浩博浩初浩宕浩歌浩广浩邈浩气浩思浩言鸿宝鸿波鸿博鸿才鸿畅鸿畴鸿达鸿德鸿飞鸿风鸿福鸿光鸿晖鸿朗鸿文鸿轩鸿煊鸿骞鸿远鸿云鸿哲鸿祯鸿志鸿卓嘉澍光济澎湃彭泽鹏池鹏海浦和浦泽瑞渊越泽博耘德运辰宇辰皓辰钊辰铭辰锟辰阳辰韦辰良辰沛晨轩晨涛晨濡晨潍鸿振吉星铭晨起运运凡运凯运鹏运浩运诚运良运鸿运锋运盛运升运杰运珧运骏运凯运乾维运运晟运莱运华耘豪星爵星腾星睿星泽星鹏星然震轩震博康震震博振强振博振华振锐振凯振海振国振平昂然昂雄昂杰昂熙昌勋昌盛昌淼昌茂昌黎昌燎昌翰晨朗德明德昌德曜范明飞昂高旻晗日昊然昊天昊苍昊英昊宇昊嘉昊明昊伟昊硕昊磊昊东鸿晖鸿朗华晖金鹏晋鹏敬曦景明景天景浩俊晖君昊昆琦昆鹏昆纬昆宇昆锐昆卉昆峰昆颉昆谊昆皓昆鹏昆明昆杰昆雄昆纶鹏涛鹏煊曦晨曦之新曦旭彬旭尧旭鹏旭东旭炎炫明宣朗学智轩昂彦昌曜坤曜栋曜文曜曦曜灿曜瑞智伟智杰智刚智阳昌勋昌盛昌茂昌黎昌燎昌翰晨朗昂然昂雄昂杰昂熙范明飞昂高朗高旻德明德昌德曜智伟智杰智刚智阳瀚彭旭炎宣朗学智昊然昊天昊苍昊英昊宇昊嘉昊明昊伟鸿朗华晖金鹏晋鹏敬曦景明景天景浩景行景中景逸景彰昆鹏昆明昆杰昆雄昆纶鹏涛鹏煊景平俊晖君昊昆琦昆鹏昆纬昆宇昆锐昆卉昆峰昆颉昆谊轩昂彦昌曜坤曜文曜曦曜灿曜瑞曦晨曦之新曦鑫鹏旭彬旭尧旭鹏旭东浩轩浩瀚浩慨浩阔鸿熙鸿羲鸿禧鸿信泽洋泽雨哲瀚胤运佑运允晨运恒运发云天耘志耘涛振荣振翱中震子辰晗昱瀚玥瀚昂瀚彭景行景中景逸景彰绍晖文景曦哲永昌子昂智宇智晖晗日晗昱瀚昂昊硕昊磊昊东鸿晖绍晖文昂文景曦哲永昌子昂智宇智晖浩然鸿运辰龙运珹振宇高朗景平鑫鹏昌淼炫明昆皓曜栋文昂"
j <- sample(1:str_count(malename),1)
substr(malename,j,j+1)

x <- data.frame()
for (k in 1:str_count(xing)) {
    x1=substr(xing,k,k)
    x2=substr(malename,k,k)
    j=sample(1:300,1)
    x3=substr(malename,k+j,k+j)
    x4=str_c(x2,x3)
    name = ifelse(k>j,
                  paste0(x1,x4),
                  paste0(x1,x3))
    x[k,1]=name
}
x$gender <- rep('M',str_count(xing))
x

femalename <- '恨桃依秋依波香巧紫萱涵易忆之幻巧美倩安寒白亦惜玉碧春怜雪听南念紫夏凌旋芷梦凌寒梦竹千凡丹蓉慧贞思菱平卉笑柳雪卉南蓉谷梦巧兰绿蝶飞荷佳蕊芷荷怀瑶慕易若芹紫安曼冬寻巧雅昕尔槐以旋初夏依丝怜南傲菡谷蕊笑槐飞兰笑卉迎荷佳音梦君妙绿觅雪寒安沛凝白容乐蓉映安依云映冬凡雁梦秋梦凡秋巧若云元容怀灵寒天薇翠安乐琴宛南怀蕊白风访波亦凝易绿夜南曼凡亦巧青易冰真白萱友安海之小蕊又琴天风若松盼菡秋荷香彤语梦惜蕊迎彤沛白雁彬易蓉雪晴诗珊春冬晴钰冰绿半梅笑容沛凝映秋盼烟晓凡涵雁问凝冬萱晓山雁蓉梦蕊山菡南莲飞双凝丝思萱怀梦雨梅冷霜向松迎丝迎梅雅彤香薇以山碧萱寒云向南书雁怀薇思菱忆文翠巧书文若山向秋凡白绮烟从天曼又亦从语绮彤之玉凡梅依琴沛槐又槐元绿安珊夏之易槐宛亦白翠丹云问寒易文傲易青旋思真雨珍幻丝代梅盼曼妙之半双若翠初兰惜萍初之宛丝寄南小萍静珊千风天蓉雅青寄文涵菱香波青亦元菱翠彤春海惜珊向薇冬灵惜芹凌青谷芹雁桃映雁书兰盼香梅致寄风芳荷绮晴映之醉波幻莲晓昕傲柔寄容以珊紫雪芷容书琴美伊涵阳怀寒易云代秋惜梦宇涵谷槐怀莲英莲芷卉向彤新巧语海灵珊凝丹小迎夏慕卉飞珍冰夏亦竹飞莲秋月元蝶春怀绿尔容小玉幼南凡梦碧菡初晴宛秋傲旋新之凡儿夏真静枫芝萱恨蕊乐双念薇靖雁菊颂丹蝶元瑶冰蝶念波迎翠海瑶乐萱凌兰曼岚若枫傲薇雅芝乐蕊秋灵凤娇觅云依伊恨山从寒忆香香菱静曼青寒笑天涵元柏代萱紫真千青雪珍寄琴绿蕊荷柳诗翠念瑶兰楠曼彤怀曼香巧采蓝芷天尔曼巧蕊'
y <- data.frame()
for (k in 1:str_count(xing)) {
  x1=substr(xing,k,k)
  x2=substr(femalename,k,k)
  j=sample(1:300,1)
  x3=substr(femalename,k+j,k+j)
  x4=str_c(x2,x3)
  name = ifelse(k>j,
                paste0(x1,x4),
                paste0(x1,x3))
  y[k,1]=name
}
y$gender <- rep('F',str_count(xing))

students <- rbind(x,y) %>%
  rename(name=V1) %>% 
  mutate(class=rep(1:9,44)) %>% 
  select(class,name,gender) %>% 
  arrange(class,name,gender)
WriteXLS(students,'students.xls')

scores <- students %>% 
  select(name) %>% 
  mutate(chinese=sample(50:100,396,replace = TRUE),
         math=sample(30:100,396,replace = TRUE),
         english=sample(40:100,396,replace = TRUE),
         physics=sample(30:100,396,replace = TRUE),
         chemistry=sample(50:100,396,replace = TRUE),
         biology=sample(50:100,396,replace = TRUE)) %>% 
  arrange(name)
scores
WriteXLS(scores,'scores.xls')

merge(students,scores,by='name') %>% 
  group_by(class) %>% 
  filter(chinese==max(chinese))

merge(students,scores,by='name') %>% 
  group_by(class) %>% 
  mutate(total=chinese+math+english+physics+chemistry+biology) %>% 
  filter(total==max(total)) %>% 
  arrange(-total)

opar <- par(no.readonly=TRUE) #生成可以修改的当前图形参数列表
par(mfrow=c(2,2))
par(lty=2, pch=17) # 修改参数
plot(dose, drugA, type="b") 
plot(dose, drugB, type="l") 
par(opar) 


## 测验
library(dplyr)
library(tidyr)
head(citydata)
dat1 <- citydata %>% 
  gather(year,gdp,-city) %>% 
  filter(city=='北京'|
           city=='上海'|
           city=='广州'|
           city=='深圳'|
           city=='杭州',
         year>=2003&year<=2017) %>% 
  spread(city,gdp) 
head(dat1)
attach(dat1)
par(family='STXihei',lwd=1.5,cex=1.2)
plot(year,北京,type='b',pch=1,lty=1,col=1,
     xlim = c(2003,2017),ylim = c(2000,31000),
     xlab='年份',ylab = 'GDP(亿元)',
     main = '五个城市经济发展比较（2003-2017）')
lines(year,上海,type='b',pch=2,lty=2,col=2)
lines(year,广州,type='b',pch=3,lty=3,col=3)
lines(year,深圳,type='b',pch=4,lty=4,col=4)
lines(year,杭州,type='b',pch=5,lty=5,col=5)
legend("topleft",c('北京','上海','广州','深圳','杭州'),
       lty = 1:5, pch=1:5,col=1:5)
detach(dat1)

#c('bejing','guangzhou','hangzhou','shanghai','shenzhen')


library(dplyr)
library(ggplot2)
mycars <- cars %>% 
  mutate(type=rep(c("Domestic","Foreign"),25))
ggplot(mycars,aes(x=speed,y=dist,col=type,pch=type))+
  geom_point(size=4)+
  labs(title="The preformance comparison of two types of cars",
       subtitle = "Note that the data were recorded in the 1920s",
       caption = "Source: Ezekiel, M. (1930) Methods of Correlation Analysis. Wiley.",
       x= "Speed (mph)", y= "Stopping distance (ft)",
       pch="Type",col="Type")+
  theme_economist()


ggplot(Salaries,aes(x=rank,fill=sex))+
  geom_bar(position = "dodge")+
  labs(title = "Salaries for Professors",
       subtitle = "The 2008-09 nine-month academic salary in a college in the U.S.",
       x="",
       caption = "Fox J. and Weisberg, S. (2011) An R Companion to Applied Regression, Second Edition Sage.")+
  theme_economist()+
  scale_fill_manual(values = c("#6794a7","#014d64"))


# 克利夫兰点图
df <- mtcars %>% 
  mutate(car=row.names(mtcars))
df
order <- sort(df$mpg,index.return=T,decreasing = F)
df$car_fator <- factor(df$car,levels = df$car[order$ix])
ggplot(df,aes(mpg,car_fator))+
  #geom_segment(aes(x=0,xend=mpg,y=car_fator,yend=car_fator))+
  geom_point(shape=21,size=3,color="black",fill="red")


library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
citydata <- read_excel("datavis/data/citydata.xls")
citypop_l <- citypop%>% 
  gather(year,pop,-city) %>% 
  filter(city=='北京'|
           city=='上海'|
           city=='广州'|
           city=='深圳'|
           city=='杭州',
         year>=2000&year<=2017)
citygdp <- read_excel("datavis/data/citygdp.xls")
citygdp_l <- citygdp%>% 
  gather(year,gdp,-city) %>% 
  filter(city=='北京'|
           city=='上海'|
           city=='广州'|
           city=='深圳'|
           city=='杭州',
         year>=2000&year<=2017)

citydata <- merge(citypop_l,citygdp_l,by=c("city","year")) %>% 
  mutate(lpop=log(pop),lgdp=log(gdp))
head(citydata)
ggplot(citydata,aes(x=lpop,y=lgdp,col=city,shape=city))+
  geom_point(size=4)+
  theme_economist()+
  theme(text = element_text(family = "STKaiti"))

library(RColorBrewer)
head(economics)
ggplot(economics,aes(x=date,y=unemploy))+
  geom_bar(aes(color=unemploy),stat="identity")+
  geom_line()+
  scale_color_gradientn(colors = brewer.pal(9,"Reds"))


library(dplyr)
library(ggplot2)
library(maptools)
library(maps)
library(RColorBrewer)
library(ggthemes)

states_map <- map_data("state")
ggplot(states_map,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="white",col="black")

crimes <- USArrests %>% 
  mutate(region=tolower(rownames(USArrests)))
head((crimes))

crimes_map <- merge(states_map,crimes,by="region") %>% 
  arrange(group,order)
head(crimes_map)

ggplot(crimes_map,aes(x=long,y=lat,group=group,fill=Murder))+
  geom_polygon(col="black")+
  scale_fill_gradientn(colors= brewer.pal(9,"Reds"))+
  labs(title="Per 100,000 residents for murder in each of the 50 US states in 1973")

p <- ggplot(crimes_map,aes(x=long,y=lat,group=group,fill=Murder))+
  geom_polygon(col="black")+
  scale_fill_gradient2(low = "green",mid="gray",high="red",
                       midpoint = median(crimes$Murder))+
  scale_x_continuous(limits =c(-125,-68))+
  labs(title="Per 100,000 residents for murder in each of the 50 US states in 1973")

crimes_map_mean <- crimes_map %>% group_by(region) %>% summarise_all(mean)
p + geom_text(data = crimes_map_mean,aes(x=long,y=lat,label=region))
  


#中国地图
china_map <- readShapePoly(file.choose())
x <- china_map@data
xs <-data.frame(x, id = seq(0:924)-1)
#添加一列新的省名，把原省名的中文格式转换为UTF-8
xs$NAME1 <- iconv(xs$NAME,"GBK", "UTF-8")

# 转换数据
china_map1 <- fortify(china_map)
head(china_map1)

#合并
library(plyr)
china_map_data <- join(china_map1, xs, type = 'full')
head(china_map_data)

ggplot(china_map_data,aes(x=long,y=lat,group=group,fill=AREA))+
  geom_polygon(col="black")+
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_light()

ggplot(china_map_data,aes(x=long,y=lat,group=group,fill=AREA))+
  geom_polygon(col="black")+
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_light()

#
qa <- quantile(china_map_data$AREA,seq(0,1,0.2))
china_map_data$area_q <- cut(china_map_data$AREA,qa,
                             labels = c("0-20%",
                                        "20-40%",
                                        "40-60%",
                                        "60-80%",
                                        "80-100%"),
                             include.lowest = TRUE)

ggplot(china_map_data,aes(x=long,y=lat,group=group,fill=area_q))+
  geom_polygon(col="black")+
  scale_fill_manual(values=brewer.pal(5,"RdBu"))+
  labs(fill="AREA")+
  theme_light()

# your_data 是你要画的dataframe，其中要有省名NAME_1
plot_data <- join(china_map_data, your_data, type = 'full')



# 2020 代码 -----------------------------------------------------------------

# 导论
5+5+7+9+10
plot(cars)
plot(1:10)
hist(rnorm(10000))
iris

j <- 0
for (i in 1:100){
  j=i+j
  }
j

# 数据处理基础

a<-1
a
is.numeric(a)
b <- 'peter'
b
c <- '2019-09-16'
c
class(c)
c <- as.Date('2019-09-16')
c
class(c)

e <- TRUE
e
class(e)

f <- c(2,4,6)
f

seq(2,4,by=0.5)
seq(1, 9, by = 2) 

1:30

lalala <- "找得到对象"
lalala

lalala <- c(1,3,5,2,4)

order <- sort(lalala,index.return=TRUE,decreasing = TRUE)
order

x <- c(1,2,3,4,5)
x[x>3]

cut <- c('差','一般','良好','优秀')
class(cut)
as.factor(cut)

df <- data.frame(
  x=c('a','b','c'),
  y=1:3,
  z=c(2,5,3)
)
df
class(df)

i <- 5
if(i>3){
  print('yes') 
} else {
  print('no')
}

ifelse(i>3,i/2,i*2)


for(i in 1:10){
  j <- i+10
  print(j)
}


mydf <- data.frame(x=c('A','B','C'),
                 '2010'=c(1,3,4),
                 '2011'=c(3,5,2),
                 check.names = FALSE)
mydf

library(tidyr)
df_gather <- gather(mydf,
                    key="year",
                    value = "score",-x)
df_gather

df_spread <- spread(df_gather,
                    key = "year",
                    value = "score")
df_spread

library(dplyr)
df_gather2 <- mutate(df_gather,
                     value2=score*2,value3=score/5)
df_gather2

df_gather <- mutate(df_gather,value=score)

df_gather %>% 
  mutate(value3=ifelse(year=='2011',
                           value*3,
                           value)) %>% 
  mutate(value4=value*100) 
  
df3 <- data.frame(x=c('lala','dada'),y=2:3)

iris %>% 
  group_by(Species) %>% 
  summarise_all(max)

iris
summary(iris)

WriteXLS(df_merge,"df_merge.xls")


library(dplyr)
scores %>% 
  filter(chinese==max(chinese)) %>% 
  select(name,chinese)


scores %>% 
  mutate(total=rowSums(scores[,-1])) %>% 
  filter(total==max(total))


library(dplyr)
library(tidyr)

#练习1：
#请整理一下全国各省近10年的GDP数据 由短数据变成长数据
head(gdpdata)
gdpdata_long <- gdpdata %>% 
  gather(key=year,value = gdp,-region)
head(gdpdata_long)
View(gdpdata_long)

#练习2：
head(students)
head(scores)
#请列出总分最高分和最低分（姓名和分数）
scores %>% 
  mutate(total=chinese+math+english+physics+chemistry+biology) %>% 
  filter(total==max(total))

scores %>% 
  mutate(total=chinese+math+english+physics+chemistry+biology) %>% 
  filter(total==min(total))

#请列出各科成绩最高分的学生（姓名和分数）
scores %>% 
  gather(key=subject,value=score,-name) %>% 
  group_by(subject) %>% 
  summarise_at("score",max)

scores %>% 
  gather(key=subject,value=score,-name) %>% 
  group_by(subject) %>% 
  filter(score==max(score))
  
#请计算各科及格率（60分为及格线）
scores %>% 
  gather(key=subject,value=score,-name) %>% 
  mutate(jige=ifelse(score>=60,1,0)) %>% 
  group_by(subject) %>% 
  summarise_at("jige",sum) %>% 
  mutate(jigelv=jige/396*100) 

#请按班级分别计算各科平均成绩
table(students$class)
scores %>% 
  merge(students,by="name") %>% 
  select(-name,-gender) %>% 
  group_by(class) %>% 
  summarise_all(mean)
  

#请列出各个班级总分最高和最低的学生（姓名和分数）
scores %>% 
  merge(students,by="name") %>% 
  mutate(total=chinese+math+english+physics+chemistry+biology) %>% 
  group_by(class) %>% 
  filter(total==max(total)) %>% 
  arrange(class)
  
scores %>% 
  merge(students,by="name") %>% 
  mutate(total=chinese+math+english+physics+chemistry+biology) %>% 
  group_by(class) %>% 
  filter(total==min(total)) %>% 
  arrange(class)

#请分班级比较男生和女生的成绩情况（总分与各科）

## 总分
scores %>% 
  merge(students,by="name") %>% 
  mutate(total=chinese+math+english+physics+chemistry+biology) %>% 
  group_by(class,gender) %>% 
  filter(total==max(total)) %>% 
  arrange(class,gender)

scores %>% 
  merge(students,by="name") %>% 
  gather(key=subject,value=score,-name,-class,-gender) %>% 
  group_by(class,gender,subject) %>% 
  filter(score==max(score)) %>% 
  arrange(class,subject,gender)

########

filter()
arrange()
head(mtcars) %>% filter(mpg==21)
arrange(head(mtcars), cyl,disp)


#练习1：
#请整理一下全国各省近10年的GDP数据 由短数据变成长数据
library(dplyr)
library(tidyr)
head(gdpdata)
gdpdata_long <- gdpdata %>% 
  gather(key=year,value=gdp,-region)
View(gdpdata_long)



#练习2：

#请列出总分最高分和最低分（姓名和分数）
head(scores)
scores %>% 
  mutate(total=chinese+math+english+physics+chemistry+biology) %>% 
  filter(total==min(total))

#请列出各科成绩最高分的学生（姓名和分数）
head(scores)
scores %>% 
  gather(key=subject,value=score,-name) %>% 
  group_by(subject) %>% 
  summarise_at("score",max)

lala <- scores %>% 
  gather(key=subject,value=score,-name) %>% 
  group_by(subject) %>% 
  filter(score==max(score))
View(lala)

#请计算各科及格率（60分为及格线）
scores %>% 
  gather(key=subject,value=score,-name) %>% 
  mutate(jige=ifelse(score>=60,1,0)) %>% 
  group_by(subject) %>% 
  summarise_at("jige",sum) %>% 
  mutate(jigelv=jige/396*100)


#请按班级分别计算各科平均成绩
head(students)
scores %>% 
  merge(students,by="name") %>% 
  select(-name,-gender) %>% 
  group_by(class) %>% 
  summarise_all(mean)

#请列出各个班级总分最高和最低的学生（姓名和分数）

scores %>% 
  merge(students,by="name") %>%
  mutate(total=chinese+math+english+physics+chemistry+biology) %>% 
  group_by(class) %>% 
  filter(total==min(total)) %>% 
  arrange(class)

#请分班级比较男生和女生的成绩情况（总分与各科）

scores %>% 
  merge(students,by="name") %>%
  mutate(total=chinese+math+english+physics+chemistry+biology) %>% 
  group_by(class,gender) %>% 
  filter(total==max(total)) %>% 
  arrange(class)


scores %>% 
  merge(students,by="name") %>% 
  gather(key=subject,value=score,-name,-class,-gender) %>% 
  group_by(class,gender,subject) %>% 
  filter(score==max(score)) %>% 
  arrange(class,subject)
  
  

## 基础绘图

mpg
mtcars$mpg
attach(mtcars)
plot(wt, mpg)
title("Regression of MPG on Weight")
abline(lm(mpg~wt),col='red')
detach(mtcars)

dose  <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
data.frame(dose,drugA,drugB)

plot(dose, drugA, type="s") 

opar <- par(no.readonly=TRUE) 
par(lty=2, pch=17)
plot(dose, drugA, type="b")
par(opar)

plot(dose, drugA, type="b",
     lty=2,pch=13,cex=3,lwd=3,col="darkblue",
     main = "this is a picture",
     sub="This is hypothetical data",
     xlab = "dose use",
     ylab="response to drug A ",
     xlim=c(18,60),ylim=c(0, 65))


opar <- par(no.readonly=TRUE)
par(lwd=2, cex=1.5)

plot(dose, drugA, type="b",pch=15, lty=1, col="red", ylim=c(0, 60),
     main="Drug A vs. Drug B",
     xlab="Drug Dosage", ylab="Drug Response")
lines(dose, drugB, type="b",pch=17, lty=2, col="blue")
abline(h=30, lwd=1.5, lty=2, col="gray")
legend("topright", title="Drug Type", c("A","B"),
       box.col = "blue", 
       lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"))


## plot绘图 作业讲解
library(dplyr)
library(tidyr)
dat1 <- citydata %>% 
  gather(year,gdp,-city) %>% 
  filter(city=="北京"|
           city=="上海"|
           city=="广州"|
           city=="深圳"|
           city=="杭州",
         year>=2003&year<=2017) %>% 
  spread(city,gdp)

attach(dat1)
par(family='STXihei',lwd=1.5,cex=1.2)
plot(year,北京,type = "b",col=1,lty=1,pch=1,
     xlab = "年份",ylab = "GDP(亿元)",
     ylim=c(2000,31000),
     main="五个城市经济发展比较（2003-2017）")
lines(year,上海,type="b",col=2,lty=2,pch=2)
lines(year,广州,type="b",col=3,lty=3,pch=3)
lines(year,深圳,type="b",col=4,lty=4,pch=4)
lines(year,杭州,type="b",col=5,lty=5,pch=5)
legend("topleft",c("北京","上海","广州","深圳","杭州"),
       col=1:5,lty=1:5,pch=1:5)
detach(dat1)


## 讲授内容

head(mtcars)
attach(mtcars)
par(mfrow=c(4,1))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs. disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
detach(mtcars)

barplot(1:10,col=1:10)
display.brewer.all()
brewer.pal(5,"Reds")
barplot(1:8,col=brewer.pal(8,"RdBu"))

df <- cbind(a=1:4,b=c(1,3,2,4))
barplot(df[,1:2])
barplot(df[,1:2],beside = T)

df
row.names(df) <- c('A','B','C','D')
barplot(df,beside = TRUE,legend.text = TRUE,col=brewer.pal(4,"RdBu"))

pie(1:10,col = rainbow(10))
rep(1, 24)
pie(rep(1, 24), col = rainbow(24))
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
    "Apple", "Boston Cream", "Other", "Vanilla Cream")

## ggplot2
library(ggplot2)
library(dplyr)
library(ggthemes)
cars
mycars <- cars %>% 
  mutate(type=rep(c("Domestic","Foreign"),25)) #生成类别变量
head(mycars)

ggplot(data = mycars,aes(x=speed,y=dist,col=type,shape=type))+
  geom_point(size=3)+
  labs(title="The preformance comparison of two types of cars",
       subtitle = "Note that the data were recorded in the 1920s",
       caption = "Source: Ezekiel, M. (1930) Methods of Correlation Analysis. Wiley.",
       x= "Speed (mph)", y= "Stopping distance (ft)")+
  theme_economist_white()

## ggplot2 其它图形的绘制
library(carData)
data("Salaries")
head(Salaries)
table(Salaries$discipline)
ggplot(Salaries,aes(x=salary,fill=sex))+
  geom_density(alpha=0.5)

library(ggthemes)
ggplot(Salaries,aes(x=rank,fill=sex))+
  geom_bar(position = "dodge")+
  theme_economist()


head(economics)
head(economics_long)

ggplot(economics,aes(date,unemploy))+
  geom_line(col="blue")

ggplot(economics_long, 
       aes(date, value01, 
           colour = variable)) +
  geom_line()


ggplot(economics,aes(x=date,y=unemploy))+
  geom_bar(aes(color=unemploy),stat = "identity")+
  geom_line()+
  scale_color_gradientn(colors = brewer.pal(9,"Reds"))

mtcars
library(dplyr)
df <- mtcars %>% 
  mutate(car=row.names(mtcars)) 

order <- sort(df$mpg,index.return=T,decreasing = F)
df$car_fator <- factor(df$car,levels = df$car[order$ix])

ggplot(df,aes(mpg,car_fator))+
  geom_point(shape=21,size=3,color="black",fill="red")

map('state', fill = TRUE, col = palette())

## 空间绘图
states_map <- map_data("state")
head(states_map)
unique(states_map$region)

ggplot(states_map,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="white",col="black")

head(USArrests)

crimes <- USArrests %>% 
  mutate(region=tolower(row.names(USArrests)))

crimes_map <- merge(states_map,crimes,by="region") %>% 
  arrange(group,order)
head(crimes_map)

p <- ggplot(crimes_map,aes(x=long,y=lat,group=group,fill=Murder))+
  geom_polygon(col="black")+
  scale_fill_gradient2(low = "green", mid="gray",high = "red",
                       midpoint = median(crimes_map$Murder))+
  labs(title="Per 100,000 residents for murder in each of the 50 US states in 1973")+
  theme(plot.title = element_text(size=10,color = "blue",hjust = 0.5))

crimes_map_mean <- crimes_map %>%
  group_by(region) %>% 
  summarise_all(mean)
  
p+
  geom_text(data=crimes_map_mean,aes(x=long,y=lat,label=region),nudge_y = 0.5)+
  geom_point(data=crimes_map_mean,aes(x=long,y=lat))


## 中国地图
library(ggthemes)
library(plyr) # 安装
library(dplyr)
library(ggplot2)
library(maptools) # 安装
library(maps) # 安装
library(RColorBrewer)

china_map <- readShapePoly(file.choose())

head(china_map1)

ggplot(china_map_data,aes(x=long,y=lat,group=group,fill=AREA))+
  geom_polygon(col="black")+
  scale_fill_gradientn(colours = brewer.pal(9,"Greens"))

unique(china_map_data$province)
province2018 <- 分省数据2018 
china_map_data_new <- china_map_data %>% 
  merge(province2018,by="province",all.x=T) %>% 
  arrange(group,order)
ggplot(china_map_data_new,aes(x=long,y=lat,group=group,fill=gdp)) +
  geom_polygon(col="black")+
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_light()

unique(china_map_data_new$province)



library(sf)
china_map <- st_read(file.choose())
class(china_map)
head(china_map)

library(ggplot2)
library(RColorBrewer)
ggplot(data = china_map)+
  geom_sf()

ggplot(data = china_map)+
  geom_sf(fill="white")+
  theme_light()

ggplot(data = china_map)+
  geom_sf(aes(fill=AREA))+
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_light()


## 课程总结

plot()
barplot(1:100)
hist(1:100)


# 2021 代码 -----------------------------------------------------------------
demo()
demo(graphics)
demo(persp)
demo(Japanese)

#向量
a <- 1:5
a

b <- "张三"
b

c <- as.Date("2021-03-09")
c
class(c)

birthday <- as.Date("1987-01-13")
c-birthday

d <- TRUE
d
5==5

plot(as.factor(1:5))
plot(1:5)

seq(1,10,length.out = 4)
seq(0, 1, length.out = 11)

rep(1:4, each = 2) 
rep(1:4, time = 2) 
rep(1:4, c(2,2,5,2))

cut(1:10,4)

e <- sample(100)
e
sort(e,decreasing = T)

library(tidyr)
mydf <- data.frame(x=c('A','B','C'),
                   '2010'=c(1,3,4),
                   '2011'=c(3,5,2),
                   "2012"=c(3,7,8),
                   check.names = FALSE)
mydf
df_gather <- gather(mydf,key="year",value = "score",-x)
spread(df_gather,year,score)


dat <- 分省年度数据
df_gather <- gather(dat,key="year",value = "gdp",-region)
head(df_gather)

spread(df_gather,year,gdp)

library(dplyr)
iris %>% 
  group_by(Species) %>% 
  summarise_all(min)

head(scores)
scores %>% 
  mutate(total=chinese+math+english+
           physics+chemistry+biology) %>% 
  filter(total==min(total)|
           total==max(total))

library(tidyr)
scores %>% 
  gather(key = subject,value = score,-name) %>%
  mutate(score=ifelse(subject=="chinese"&
                        score==100,99,score)) %>% 
  group_by(subject) %>% 
  filter(score==min(score)) %>% 
  View()


scores %>% 
  gather(key = subject,value = score,-name) %>% 
  mutate(jige=ifelse(score>=60,1,0)) %>% 
  group_by(subject) %>% 
  summarise_at("jige",sum) %>% 
  mutate(jigelv=jige/396*100)


scores %>% 
  gather(key = subject,value = score,-name) %>%
  group_by(subject) %>%
  summarise_at("score",list(mean,sd,min,max))





  

