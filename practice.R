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

############################

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

attach(dat1)
par(family='STKaiti',lwd=1.5,cex=1.2)
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


