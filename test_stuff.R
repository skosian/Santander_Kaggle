a=as.data.frame(c('123.56','0.2','10'))
names(a)=c('x')
str(a)
#a$x=as.numeric(a$x)

a$x=as.numeric(as.character(a$x))


b=as.data.frame(c(0,1,0,1,1,0,NA))
names(b)=c('var')
str(b)
b$var=as.integer(b$var)

b$var=as.factor(b$var)
