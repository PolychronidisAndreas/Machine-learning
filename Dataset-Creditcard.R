#########################Τελική Εργασία####################################################################



#####Εγκαθηστούμε το πακέτο με το οποίο θα ασχοληθούμε σε αυτή την εργασία.

library(AER)
CreditCard <- read.csv("D:/Downloads/CreditCard.csv", encoding="UTF-8", comment.char="#")

#####Βλέπουμε τη δομή του dataset και τις μεταβλητές του.

str(CreditCard)
help(CreditCard)

#Μας προκύπτει ένα σύνολο δεδομένων με 1319 παρατηρήσεις 12 μεταβλητών.

#####Βρίσκουμε το είδος της κάθε μεταβλητής.

class(CreditCard$card)          #Ποιοτική μεταβλητή
class(CreditCard$reports)       #Ποσοτική μεταβλητή
class(CreditCard$age)           #Ποσοτική μεταβλητή
class(CreditCard$income)        #Ποσοτική μεταβλητή
class(CreditCard$share)         #Ποσοτική μεταβλητή
class(CreditCard$expenditure)   #Ποσοτική μεταβλητή
class(CreditCard$owner)         #Ποιοτική μεταβλητή
class(CreditCard$selfemp)       #Ποιοτική μεταβλητή
class(CreditCard$dependents)    #Ποσοτική μεταβλητή
class(CreditCard$months)        #Ποσοτική μεταβλητή
class(CreditCard$majorcards)    #Ποσοτική μεταβλητή
class(CreditCard$active)        #Ποσοτική μεταβλητή

#########################################################################################################

#############Μελετούμε τις ποιοτικές μεταβλητές του συνόλου.#########################################

###Για την μεταβλητή card,

###Πίνακας συχνοτήτων

freq.card=table(CreditCard$card)
freq.card.df=as.data.frame(freq.card)
colnames(freq.card.df)=c("card","Frequency")
freq.card.df

###Πίνακας σχετικών συχνοτήτων

rel.freq.card=prop.table(freq.card)
rel.freq.card.df=as.data.frame(rel.freq.card)
colnames(rel.freq.card.df)=c("card","Relative Frequency")
rel.freq.card.df

###Τελικός πίνακας συχνοτήτων-σχετικών συχνοτήτων για την μεταβλητή card.

card.df=cbind(freq.card.df,rel.freq.card.df[2])
card.df   

###Ραβδόγραμμα

barplot(freq.card,main=" Card Acceptance Distribution",xlab="Card Acceptance",ylab="Frequency",horiz=FALSE,cex.names=0.8 )


###Για την μεταβλητή owner,

###Πίνακας συχνοτήτων

freq.owner=table(CreditCard$owner)
freq.owner.df=as.data.frame(freq.owner)
colnames(freq.owner.df)=c("owner","Frequency")
freq.owner.df

###Πίνακας σχετικών συχνοτήτων

rel.freq.owner=prop.table(freq.owner)
rel.freq.owner.df=as.data.frame(rel.freq.owner)
colnames(rel.freq.owner.df)=c("owner","Realitve Frequency")
rel.freq.owner.df

###Τελικός πίνακας συχνοτήτων-σχετικών συχνοτήτων για την μεταβλητή owner.

owner.df=cbind(freq.owner.df,rel.freq.owner.df[2])
owner.df   

###Ραβδόγραμμα

barplot(freq.owner,main=" Home-Owning Distribution",xlab="Owning your Home",ylab="Frequency",horiz=FALSE,cex.names=0.8 )


###Για την μεταβλητή selfemp,

###Πίνακας συχνοτήτων

freq.selfemp=table(CreditCard$selfemp)
freq.selfemp.df=as.data.frame(freq.selfemp)
colnames(freq.selfemp.df)=c("selfemp","Frequency")
freq.selfemp.df

###Πίνακας σχετικών συνοτήτων

rel.freq.selfemp=prop.table(freq.selfemp)
rel.freq.selfemp.df=as.data.frame(rel.freq.selfemp)
colnames(rel.freq.selfemp.df)=c("selfemp","Relative Frequency")
rel.freq.selfemp.df

###Τελικός πίνακας συχνοτήτων-σχετικών συχνοτήτων για την μεταβλητή selfemp.

selfemp.df=cbind(freq.selfemp.df,rel.freq.selfemp.df[2])
selfemp.df

###Ραβδόγραμμα

barplot(freq.selfemp,main="Self-Employed Distribution",xlab="Self-Employed",ylab="Frequency")


###Κατασκευάζουμε Crosstabulation Matrix για τις ποιοτικές μεταβλητές ανά ζεύγη###
###και εξετάζουμε με χρήση του chi-squared contingency table test την εξάρτησή τους.

###Για τις μεταβλητές card και owner,

dok1<-xtabs(~card+owner,CreditCard) 
dok1

chisq.test(dok1)

###Ως αποτέλεσμα του ελέγχου προκύπτει ότι p-value<0.05 ,άρα οι μεταβλητές 
###card και owner είναι εξαρτημένες.

###Για τις μεταβλητές card και selfemp,

dok2<-xtabs(~card+selfemp,CreditCard)
dok2

chisq.test(dok2)

###Ως αποτέλεσμα του ελέγχου προκύπτει ότι p-value>0.05 ,άρα οι μεταβλητές
###card και selfemp είναι ανεξάρτητες.

###Για τις μεταβλητές owner και selfemp,

dok3<-xtabs(~owner+selfemp,CreditCard)
dok3

chisq.test(dok3)

###Ως αποτέλεσμα του ελέγχου προκύπτει ότι p-value>0.05,άρα οι μεταβλητές
###owner και selfemp είναι ανεξάρτητες.

################################################################################################################

###########Μελετούμε τις ποσοτικές μεταβλητές του συνόλου.############################################

###Μέτρα Κεντρικής Τάσης###


###Μέση Τιμή

mean(CreditCard$reports)
mean(CreditCard$age)
mean(CreditCard$income)
mean(CreditCard$share)
mean(CreditCard$expenditure)
mean(CreditCard$dependents)
mean(CreditCard$months)
mean(CreditCard$majorcards)
mean(CreditCard$active)

###Διάμεσος

median(CreditCard$reports)
median(CreditCard$age)
median(CreditCard$income)
median(CreditCard$share)
median(CreditCard$expenditure)
median(CreditCard$dependents)
median(CreditCard$months)
median(CreditCard$majorcards)
median(CreditCard$active)

###Επικρατούσα τιμή

library(DescTools)

Mode(CreditCard$reports)
Mode(CreditCard$age)
Mode(CreditCard$income)
Mode(CreditCard$share)
Mode(CreditCard$expenditure)
Mode(CreditCard$dependents)
Mode(CreditCard$months)
Mode(CreditCard$majorcards)
Mode(CreditCard$active)

###Εκατοστημόρια

quantile(CreditCard$reports,c(0.25,0.75))
quantile(CreditCard$age,c(0.25,0.75))
quantile(CreditCard$income,c(0.25,0.75))
quantile(CreditCard$share,c(0.25,0.75))
quantile(CreditCard$expenditure,c(0.25,0.75))
quantile(CreditCard$dependents,c(0.25,0.75))
quantile(CreditCard$months,c(0.25,0.75))
quantile(CreditCard$majorcards,c(0.25,0.75))
quantile(CreditCard$active,c(0.25,0.75))

###Μέτρα Διασποράς

###Εύρος

range(CreditCard$reports)
range(CreditCard$age)
range(CreditCard$income)
range(CreditCard$share)
range(CreditCard$expenditure)
range(CreditCard$dependents)
range(CreditCard$months)
range(CreditCard$majorcards)
range(CreditCard$active)

###Διασπορά

var(CreditCard$reports)
var(CreditCard$age)
var(CreditCard$income)
var(CreditCard$share)
var(CreditCard$expenditure)
var(CreditCard$dependents)
var(CreditCard$months)
var(CreditCard$majorcards)
var(CreditCard$active)

###Τυπική Απόκλιση

sd(CreditCard$reports)
sd(CreditCard$age)
sd(CreditCard$income)
sd(CreditCard$share)
sd(CreditCard$expenditure)
sd(CreditCard$dependents)
sd(CreditCard$months)
sd(CreditCard$majorcards)
sd(CreditCard$active)

###Λοξότητα

library(e1071)

m1<-sum((CreditCard$reports-mean(CreditCard$reports))^3)/length(CreditCard$reports)
m1
s1<-sqrt(var(CreditCard$reports))^3
s1

skew1<-(m1+s1+m1/s1)
skew1


m2<-sum((CreditCard$age-mean(CreditCard$age))^3)/length(CreditCard$age)
m2
s2<-sqrt(var(CreditCard$age))^3
s2

skew2<-(m2+s2+m2/s2)
skew2


m3<-sum((CreditCard$income-mean(CreditCard$income))^3)/length(CreditCard$income)
m3
s3<-sqrt(var(CreditCard$income))^3
s3

skew3<-(m3+s3+m3/s3)
skew3


m4<-sum((CreditCard$share-mean(CreditCard$share))^3)/length(CreditCard$share)
m4
s4<-sqrt(var(CreditCard$share))^3
s4

skew4<-(m4+s4+m4/s4)
skew4


m5<-sum((CreditCard$expenditure-mean(CreditCard$expenditure))^3)/length(CreditCard$expenditure)
m5
s5<-sqrt(var(CreditCard$expenditure))^3
s5

skew5<-(m5+s5+m5/s5)
skew5


m6<-sum((CreditCard$dependents-mean(CreditCard$dependents))^3)/length(CreditCard$dependents)
m6
s6<-sqrt(var(CreditCard$dependents))^3
s6

skew6<-(m6+s6+m6/s6)
skew6


m7<-sum((CreditCard$months-mean(CreditCard$months))^3)/length(CreditCard$months)
m7
s7<-sqrt(var(CreditCard$months))^3
s7

skew7<-(m7+s7+m7/s7)
skew7


m8<-sum((CreditCard$majorcards-mean(CreditCard$majorcards))^3)/length(CreditCard$majorcards)
m8
s8<-sqrt(var(CreditCard$majorcards))^3
s8

skew8<-(m8+s8+m8/s8)
skew8


m9<-sum((CreditCard$active-mean(CreditCard$active))^3)/length(CreditCard$active)
m9
s9<-sqrt(var(CreditCard$active))^3
s9

skew9<-(m9+s9+m9/s9)
skew9


###Κύρτωση


M1<-sum((CreditCard$reports-mean(CreditCard$reports))^4)/length(CreditCard$reports)
M1
S1<-var(CreditCard$reports)^2
S1

kurtosis1<-(M1+S1+M1/S1 - 3)
kurtosis1 


M2<-sum((CreditCard$age-mean(CreditCard$age))^4)/length(CreditCard$age)
M2
S2<-var(CreditCard$age)^2
S2

kurtosis2<-(M2+S2+M2/S2 - 3)
kurtosis2 


M3<-sum((CreditCard$income-mean(CreditCard$income))^4)/length(CreditCard$income)
M3
S3<-var(CreditCard$income)^2
S3

kurtosis3<-(M3+S3+M3/S3 - 3)
kurtosis3 


M4<-sum((CreditCard$share-mean(CreditCard$share))^4)/length(CreditCard$share)
M4
S4<-var(CreditCard$share)^2
S4

kurtosis4<-(M4+S4+M4/S4 - 3)
kurtosis4 


M5<-sum((CreditCard$expenditure-mean(CreditCard$expenditure))^4)/length(CreditCard$expenditure)
M5
S5<-var(CreditCard$expenditure)^2
S5

kurtosis5<-(M5+S5+M5/S5 - 3)
kurtosis5 


M6<-sum((CreditCard$dependents-mean(CreditCard$dependents))^4)/length(CreditCard$dependents)
M6
S6<-var(CreditCard$dependents)^2
S6

kurtosis6<-(M6+S6+M6/S6 - 3)
kurtosis6 


M7<-sum((CreditCard$months-mean(CreditCard$months))^4)/length(CreditCard$months)
M7
S7<-var(CreditCard$months)^2
S7

kurtosis7<-(M7+S7+M7/S7 - 3)
kurtosis7 


M8<-sum((CreditCard$majorcards-mean(CreditCard$majorcards))^4)/length(CreditCard$majorcards)
M8
S8<-var(CreditCard$majorcards)^2
S8

kurtosis8<-(M8+S8+M8/S8 - 3)
kurtosis8 


M9<-sum((CreditCard$active-mean(CreditCard$active))^4)/length(CreditCard$active)
M9
S9<-var(CreditCard$active)^2
S9

kurtosis9<-(M9+S9+M9/S9 - 3)
kurtosis9 


###Διαγράμματα Διασποράς

###Για τις μεταβλητές age και income,


pairs(~age+income,data=CreditCard,main="CreditCard ScatterplotMatrix",col="orange")


###Για τις μεταβλητές income και expenditure,


pairs(~income+expenditure,data=CreditCard,main="CreditCardScatterplot Matrix",col="blue")


###Για τις μεταβλητές share και expenditure,


pairs(~share+expenditure,data=CreditCard,main="CreditCardScatterplot Matrix",col="red")


###Για τις μεταβλητές dependents και majorcards,


pairs(~dependents+majorcards,data=CreditCard,main="CreditCardScatterplot Matrix",col="purple")



###Ιστόγραμμα


hist(CreditCard$reports,breaks="scott",main="Histogram of Reports",xlab="Reports",ylab="Frequency",col="blue" )

hist(CreditCard$age,breaks="scott",main="Histogram of Age",xlab="Age",ylab="Frequency",col="green" )

hist(CreditCard$income,breaks="scott",main="Histogram of Yearly Income",xlab="Income",ylab="Frequency",col="orange" )

hist(CreditCard$share,breaks="scott",main="Histogram of share",xlab="Share",ylab="Frequency",col="purple" )

hist(CreditCard$expenditure,breaks="scott",main="Histogram of expenditure",xlab="Expenditure",ylab="Frequency",col="brown" )

hist(CreditCard$dependents,breaks="scott",main="Histogram of Number of dependents",xlab="Dependents",ylab="Frequency",col="red" )

hist(CreditCard$months,breaks="scott",main="Histogram of Months living at one address",xlab="Months",ylab="Frequency",col="grey" )

hist(CreditCard$majorcards,breaks="scott",main="Histogram of majorcards",xlab="Majorcards",ylab="Frequency",col="yellow" )

hist(CreditCard$active,breaks="scott",main="Histogram of Number of active credit accounts",xlab="Active",ylab="Frequency",col="lightblue" )


###Υπολογίζουμε τον συντελεστή συσχέτισης. 

library(gginference)

###Για τις μεταβλητές age και income,


cor(CreditCard$age,CreditCard$income)


###Αφού cor=0.3246532, άρα έχουμε ασθενή και θετική γραμμική 
###συσχέτιση μεταξύ των μεταβλητών.


###Για τις μεταβλητές income και expenditure,


cor(CreditCard$income,CreditCard$expenditure)


###Αφού cor=0.281104, προκύπτει ότι δεν έχουμε κάποια 
###μορφή γραμμικής συσχέτισης μεταξύ των συγκεκριμένων μεταβλητών.


###Για τις μεταβλητές share και expenditure,


cor(CreditCard$share,CreditCard$expenditure)


###Αφού cor=0.8387793, προκύπτει ότι έχουμε πολύ ισχυρή 
###γραμμική συσχέτιση μεταξύ των μεταβλήτων.


###Για τις μεταβλητές dependents και majorcards,


cor(CreditCard$dependents,CreditCard$majorcards)


###Αφού cor=0.01028454, προκύπτει ότι δεν έχουμε 
###γραμμική συσχέτιση μεταξύ των μεταβλήτων.


##############################################################################


###Υπολογίζουμε την ευθεία γραμμικής παλινδρόμησης


###Για τις μεταβλητές age και income,


lm1<-lm(formula=income~age,data=CreditCard)
lm1


###Άρα έχουμε την ευθεία y=1.56460+0.05422x
###με y = το ετήσιο εισόδημα
###και x = η ηλικία του ανθρώπου


###Για τις μεταβλητές share και expenditure,


lm2<-lm(formula=expenditure~share,data=CreditCard)
lm2


###Άρα έχουμε την ευθεία y=19.26+2412.24x
###με y = ο μέσο όρος των μηνιαίων εξόδων των πιστωτικών καρτών
###και x = αναλογία μηνιαίων εξόδων των πιστωτικών καρτών με το ετήσιο εισόδημα


###Ανάλυση διακύμανσης


###Εξετάζουμε αν η απάντηση στην αίτηση έκδοσης πιστωτικής
###κάρτας επηρεάζεται από  το ετήσιο εισόδημα των ατόμων
###του δείγματος.


card<-factor(card)
income.df<-data.frame(card,income)
income.df

par(mfrow=c(1,2))
plot.design(income.df)

boxplot(income~card,income.df)

qqnorm(income.df$income)
qqline(income.df$income)

ks.test(income.df$income,"pnorm")

aov.income<-aov(income~card,income.df)
aov.income
summary(aov.income)

anova(aov.income)


####################################################################################################################


###Συνεχίζουμε με τις μετρικές αξιολόγησης.


library(ISLR)
library(Metrics)
library(boot)


###Βρίσκουμε το μέσο τετραγωνικό σφάλμα για τις ποσοτικές μεταβλητές 
###του συνόλου μας.


mse(CreditCard$reports,mean(CreditCard$reports))
mse(CreditCard$age,mean(CreditCard$age))
mse(CreditCard$income,mean(CreditCard$income))
mse(CreditCard$share,mean(CreditCard$share))
mse(CreditCard$expenditure,mean(CreditCard$expenditure))
mse(CreditCard$dependents,mean(CreditCard$dependents))
mse(CreditCard$months,mean(CreditCard$months))
mse(CreditCard$majorcards,mean(CreditCard$majorcards))
mse(CreditCard$active,mean(CreditCard$active))


###Εφαρμόζουμε τη μέθοδο Hold-out.


library(ISLR)
library(Metrics)
set.seed(1)
train=sample(1319,659)

lm.fit=lm(formula=age~income,data=CreditCard,subset=train)
summary(lm.fit)


predicted<-predict(lm.fit,CreditCard)[-train]

mse(predicted,age[-train])

###Μπορούμε να χρησιμοποιήσουμε την εντολή poly για να εκτιμηθεί
###το σφάλμα ελέγχου για τις γραμμικές παλινδρομήσεις. Φυσικά, ο βαθμός 
###του πολυωνύμου μπορεί να είναι και μεγαλύτερος του 2 αν το θέλουμε.

lm.fit2=lm(age~poly(income,2),data=CreditCard,subset=train)
summary(lm.fit2)

predicted2<-predict(lm.fit2,CreditCard)[-train]
mse(predicted2,age[-train])


###Εφαρμόζουμε τη μέθοδο Leave-one-out Cross Validation.


library(boot)
glm.fit2=glm(age~income,data=CreditCard)
summary(glm.fit2)

cv.err=cv.glm(CreditCard,glm.fit2)
cv.err$delta

cv.error2=rep(0,5)
for (i in 1:5){glm.fit2=glm(age~poly(income,i),data=CreditCard)
cv.error2[i]=cv.glm(CreditCard,glm.fit2)$delta[1]}
cv.error2

predicted3<-predict(glm.fit2,CreditCard)[-train]
mse(predicted3,age[-train])



###Ελάχιστο σφάλμα έχει το 5ο μοντέλο, άρα είναι το βέλτιστο

###Εφαρμόζουμε τη μέθοδο 10-Fold Cross Validation.


###Χωρίζουμε το σύνολο δεδομένων μας σε 80% train και 20% test.

set.seed(1)
Train=sample(1319,0.8*1319)
Train
CreditCardTrain <-CreditCard[Train,]
CreditCardTrain
CreditCardTest <-CreditCard[-Train,]
CreditCardTest


###Εκπαιδεύουμε το 80% του συνόλου με τη μέθοδο 10-cross-validation
###και βρίσκουμε το βέλτιστο μοντέλο προκειμένου να καταλήξουμε στο
###βέλτιστο αποτέλεσμα, δηλαδή το μικρότερο μέσο τετραγωνικό
###σφάλμα για το δείγμα.


glm.fit0=glm(age~income,data=CreditCard)
summary(glm.fit0)

cv.err0=cv.glm(CreditCard,glm.fit0,K=10)
cv.err0$delta

cv.error1=rep(0,10)
for (i in 1:10){lm.fit=glm(age~poly(income,i),data=CreditCardTrain)  
     cv.error1[i]=cv.glm(CreditCardTrain,lm.fit)$delta[1]}


cv.error1

min(cv.error1)

glm.fit=glm(age~poly(income,5),data=CreditCardTest)
predicted<-predict(glm.fit0,CreditCardTest)
mse(predicted,CreditCardTest$age)


##########################################################################################


###Δέντρα ταξινόμησης

library(ISLR)
library(tree)
str(CreditCard)
View(CreditCard)

young=ifelse(CreditCard$age<45,"Yes","No")

CreditCardD<-data.frame(CreditCard[-4],young)
CreditCardD<-data.frame(CreditCardD[-1])
CreditCardD<-data.frame(CreditCardD[-1])
CreditCardD<-data.frame(CreditCardD[-5])
CreditCardD<-data.frame(CreditCardD[-5])


CreditCardD$young<-as.factor(CreditCardD$young)
str(CreditCardD)
View(CreditCardD)

###Χωρίζουμε τις παρατηρήσεις του δείγματος σε σύνολο εκπαίδευσης (training set-80%) 
###και σε σύνολο επικύρωσης (validation set-20%).


s_size <- floor(0.8 * nrow(CreditCardD))
set.seed(1)
train_index <- sample(1:nrow(CreditCardD),size=s_size)
train_index

train1 <- CreditCardD[train_index,]
train1

test1 <- CreditCardD[-train_index,]
test1


###Βρίσκουμε το δέντρο πρόβλεψης για την μεταβλητή young
###που ορίσαμε παραπάνω για την ηλικία. Η πρόβλεψη γίνεται
###με βάση τις μεταβλητές του συνόλου train.


tree.young <- tree(young~.,train1)


###τα στατιστικά ποτελέσματα του δέντρου δίνονται με χρήση της 
###εντολής summary.

summary(tree.young)


###Κατασκευή του δέντρου


par(mar=c(3,1,3,1))
plot(tree.young)
text(tree.young,pretty=0,cex=0.85)


###Εξετάζουμε τις προβλέψεις για την "νεανικότητα" του 
###συνόλου δεδομένων test


tree.pred1 <- predict(tree.young,test1,type="class")
tree.pred1

###Κατασκευάζουμε το πίνακα σύγχησης (Confusion Matrix), προκειμένου
###να αξιολογήσουμε το μοντέλο μας


ConMat<-table(tree.pred1,test1$young)
ConMat


library(caret)
confusionMatrix(ConMat)


###Βελτιστοποιούμε το μοντέλο κλαδεύοντας το δέντρο ταξινόμησης
###χρησιμοποιώντας το σφάλμα ταξινόμησης ως κριτήριο του Cross-Validation 


cv.young <- cv.tree(tree.young,FUN=prune.misclass)
names(cv.young)
cv.young


###Παρατηρούμε ότι το βέλτιστο μοντέλο ταξινόμησης προκύπτει 
###με 3 τερματικούς κόμβους


prune.young <- prune.misclass(tree.young,best=3)


plot(prune.young)
text(prune.young,pretty=0,cex=0.85)



tree.pred2<-predict(prune.young,test1,type="class")

ConMat2<-table(tree.pred2,test1$young)
ConMat2

confusionMatrix(ConMat2)

###Παρατηρούμε την αύξυση της ευστοχίας (accuracy) της ταξινόμησης
###καθώς από το αρχικό 85.61% , έχουμε εν τέλει 86.74% ευστοχία.



###Support Vector Machine###

library(e1071)
library(caret)
library(tidyverse)


###Χωρίζουμε το dataset σε 80% train και 20% test.

set.seed(123)

training.samples <- CreditCardD$young %>% createDataPartition(p=0.8,list=FALSE)
train.data <- CreditCardD[training.samples,]
test.data <- CreditCardD[-training.samples,]


###Χρησιμοποιούμε γραμμικό ταξινομητή για την πρώτη μέθοδο SVM

set.seed(123)
svmfit1=svm(train.data$young~.,data=train.data,kernel="linear",cost=100,scale=FALSE)
summary(svmfit1) 

###Χρησιμοποιούμε το test σύνολο για να κάνουμε προβλέψεις για
###την ευστοχία του μοντέλου

predicted.classes1 <- svmfit1 %>% predict(test.data)          
head(predicted.classes1)

###Υπολογίζουμε το βαθμό ευστοχίας του μοντέλου 

mean(predicted.classes1 == test.data$young)

###Χρησιμοποιούμε μη γραμμικό ταξινομητή για τη δεύτερη μέθοδο SVM

set.seed(123)
svmfit2=svm(train.data$young~.,data=train.data,kernel="radial",gamma=1,cost=100)
summary(svmfit2)

###Χρησιμοποιούμε το test σύνολο για να κάνουμε προβλέψεις για
###την ευστοχία του μοντέλου

predicted.classes2 <- svmfit2 %>% predict(test.data)
head(predicted.classes2)  

###Υπολογίζουμε το βαθμό ευστοχίας του μοντέλου

mean(predicted.classes2 == test.data$young)

###Παρατηρούμε ότι έχουμε μεγαλύτερη ευστοχία με τη χρήση μη γραμμικού 
###ταξινομητή με την πρώτη προσπάθεια κόστους 100 και γάμμα=1

###Θα βρούμε τώρα ανάμεσα σε πολλαπλά μοντέλα το βέλτιστο
###με βάση τις παραμέτρους κόστους και γάμμα

library(MASS)

###Για τον γραμμικό ταξινομητή,


set.seed(123)
tune.out1=tune.svm(young~.,data=train.data,kernel="linear",
                   cost=c(1,10,100,1000,10000))

summary(tune.out1)

###Βρίσκουμε το βέλτιστο μοντέλο

best.tune1 =tune.out1$best.model
summary(best.tune1)
tune.test1=predict(best.tune1,newdata=test.data)

###Κατασκευάζουμε τον πίνακα σύγχυσης για να δούμε την ανταπόκριση
###στην κατανομή των τιμών για την πρόβλεψη

table(tune.test1,test.data$young)

confusionMatrix(tune.test1,test.data$young)


###Για τον μη γραμμικό ταξινομητή,


set.seed(123)
tune.out2=tune.svm(young~.,data=train.data,kernel="radia",
                   cost=c(1,10,100,1000,10000),gamma=c(1,2,3,4,5))
summary(tune.out2)

###Βρίσκουμε το βέλτιστο μοντέλο

best.tune2=tune.out2$best.model
tune.test2=predict(best.tune2,newdata=test.data)

###Κατασκευάζουμε τον πίνακα σύγχυσης για να δούμε την ανταπόκριση
###στην κατανομή των τιμών για την πρόβλεψη

confusionMatrix(tune.test2,test.data$young)


###Με μικρή διαφορά συμπεραίνουμε πως ο γραμμικός πυρήνας αποτελεί την 
###καλύερη επιλογή για την πρόβλεψη ταξινόμησης των σημείων του συνόλου,
###καθώς έχουμε μεγαλύτερη ευστοχία (87%) έναντι του μη γραμμικού (86%).



#######################################################################################################
