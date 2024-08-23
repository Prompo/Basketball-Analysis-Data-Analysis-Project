#οι εντολές που χρησιμοποιήθηκαν για τη διαμόρφωση των δεδομένων
#data <- read.table("NBA.txt", header = TRUE)
#elements<- strsplit(data[1,1],"\\s+)[[1]])) #split the string
#combined<- paste0(elements[1],elements[2]) #combine strings
#cat(combined,"\n") # to remove ""


#Δημιουργία μεταβλητών
Player<- character(0)
Points<-numeric(0)
Assists<- numeric(0)
Rebounds<- numeric(0)
TPP<- numeric(0)
FT<- numeric(0)
Games<- numeric(0)
Minutes<- numeric(0)
Height<- numeric(0)
Age<- numeric(0)

#Αυτή τη στιγμή τα στοιχεία υπάρχουν ως αριθμοί αλλά σε μορφή συμβολοσειράς. Για να τους κάνουμε αριθμούς
#χρησιμοποιούμε την εντολή a.numeric(Element[i])
#Μέσα σε ένα επαναληπτικό loop 105 επαναλήψεων περνάμε χειροκίνητα τα δεδομένα στις λίστες.

#Δημιουργώ τη μεταβλητή PPM
for (i in 1:105){
PPM[i]<- Points[i]/48
}

#Μετρέπω τη ποσοτική μεταβλητή Age, σε κατηγορική 4 μεταβλητών
Age4<- cut(Age, breaks = 4)

#Δημιουργώ τη μεταβλητή avg_game_time
avg_game_time<- numeric(0)
for (i in 1:105){
avg_game_time[i]= Minutes[i]/Games[i]
}

#Δημιουργώ τη μεταβλητή over20
over20<- numeric(0)
for (i in 1:105){
if (avg_game_time[i]>20){
over20[i]=1
}else{
over20[i]=0
}
}

#Σε αυτό το σημείο εκτελούμε τις εντολές προκειμένουμε να υλοποιήσουμε τη περιγραφική ανάλυση της εργασίας

#περιγραφική ανάλυση της μεταβλητής Points
mean(Points)#υπολογισμός μέσης τιμής
sd(Points) #υπολογισμός τυπικής απόκλισης
median(Points) #υπολογισμός διαμέσου
min(Points) #εύρεση μικρότερης  τιμής
max(Points) #εύρεση μεγαλύτερης τιμής
skewness(Points) #υπολογισμός ασσυμετρίας
kurtosis(Points) #υπολογισμός κύρτωσης
 
 #περιγραφική ανάλυση της μεταβλητής Assists
mean(Assists)#υπολογισμός μέσης τιμής
sd(Assists) #υπολογισμός τυπικής απόκλισης
median(Assists) #υπολογισμός διαμέσου
min(Assists) #εύρεση μικρότερης  τιμής
max(Assists) #εύρεση μεγαλύτερης τιμής
skewness(Assists) #υπολογισμός ασσυμετρίας
kurtosis(Assists) #υπολογισμός κύρτωσης
 
 #περιγραφική ανάλυση της μεταβλητής Rebounds
mean(Rebounds)#υπολογισμός μέσης τιμής
sd(Rebounds) #υπολογισμός τυπικής απόκλισης
median(Rebounds) #υπολογισμός διαμέσου
min(Rebounds) #εύρεση μικρότερης  τιμής
max(Rebounds) #εύρεση μεγαλύτερης τιμής
skewness(Rebounds) #υπολογισμός ασσυμετρίας
kurtosis(Rebounds) #υπολογισμός κύρτωσης
 
 #περιγραφική ανάλυση της μεταβλητής Minutes
mean(Minutes)#υπολογισμός μέσης τιμής
sd(Minutes) #υπολογισμός τυπικής απόκλισης
median(Minutes) #υπολογισμός διαμέσου
min(Minutes) #εύρεση μικρότερης  τιμής
max(Minutes) #εύρεση μεγαλύτερης τιμής
skewness(Minutes) #υπολογισμός ασσυμετρίας
kurtosis(Minutes) #υπολογισμός κύρτωσης
 
 #περιγραφική ανάλυση της μεταβλητής Height
mean(Height)#υπολογισμός μέσης τιμής
sd(Height) #υπολογισμός τυπικής απόκλισης
median(Height) #υπολογισμός διαμέσου
min(Height) #εύρεση μικρότερης  τιμής
max(Height) #εύρεση μεγαλύτερης τιμής
skewness(Height) #υπολογισμός ασσυμετρίας
kurtosis(Height) #υπολογισμός κύρτωσης
 
 #περιγραφική ανάλυση της μεταβλητής Games
mean(Games)#υπολογισμός μέσης τιμής
sd(Games) #υπολογισμός τυπικής απόκλισης
median(Games) #υπολογισμός διαμέσου
min(Games) #εύρεση μικρότερης  τιμής
max(Games) #εύρεση μεγαλύτερης τιμής
skewness(Games) #υπολογισμός ασσυμετρίας
kurtosis(Games) #υπολογισμός κύρτωσης
 
 #περιγραφική ανάλυση της μεταβλητής TPP
mean(TPP)#υπολογισμός μέσης τιμής
sd(TPP) #υπολογισμός τυπικής απόκλισης
median(TPP) #υπολογισμός διαμέσου
min(TPP) #εύρεση μικρότερης  τιμής
max(TPP) #εύρεση μεγαλύτερης τιμής
skewness(TPP) #υπολογισμός ασσυμετρίας
kurtosis(TPP) #υπολογισμός κύρτωσης
 
  #περιγραφική ανάλυση της μεταβλητής PPM
mean(PPM)#υπολογισμός μέσης τιμής
sd(PPM) #υπολογισμός τυπικής απόκλισης
median(PPM) #υπολογισμός διαμέσου
min(PPM) #εύρεση μικρότερης  τιμής
max(TPP) #εύρεση μεγαλύτερης τιμής
skewness(PPM) #υπολογισμός ασσυμετρίας
kurtosis(PPM) #υπολογισμός κύρτωσης

 #περιγραφική ανάλυση της μεταβλητής FT
mean(FT)#υπολογισμός μέσης τιμής
sd(FT) #υπολογισμός τυπικής απόκλισης
median(FT) #υπολογισμός διαμέσου
min(FT) #εύρεση μικρότερης  τιμής
max(FT) #εύρεση μεγαλύτερης τιμής
skewness(FT) #υπολογισμός ασσυμετρίας
kurtosis(FT) #υπολογισμός κύρτωσης

 #περιγραφική ανάλυση της μεταβλητής Age
mean(Age)#υπολογισμός μέσης τιμής
sd(Age) #υπολογισμός τυπικής απόκλισης
median(Age) #υπολογισμός διαμέσου
min(Age) #εύρεση μικρότερης  τιμής
max(Age) #εύρεση μεγαλύτερης τιμής
skewness(Age) #υπολογισμός ασσυμετρίας
kurtosis(Age) #υπολογισμός κύρτωσης

 #περιγραφική ανάλυση της μεταβλητής avg_game_time
mean(avg_game_time)#υπολογισμός μέσης τιμής
sd(avg_game_time) #υπολογισμός τυπικής απόκλισης
median(avg_game_time) #υπολογισμός διαμέσου
min(avg_game_time) #εύρεση μικρότερης  τιμής
max(avg_game_time) #εύρεση μεγαλύτερης τιμής
skewness(avg_game_time) #υπολογισμός ασσυμετρίας
kurtosis(avg_game_time) #υπολογισμός κύρτωσης
 
 #παρακάτω ακολουθεί ο κώδικας με τις εντολές που χρησιμοποίησα για να ελέγξω αν οι μεταβλητές ακολοθούν κανονική κατανομή.
 #Η εντολή αντιπροσωπεύει shapiro.test() τον έλεγχο Shapiro-Wilk , ενώ η εντολή lillie.test() αντιπροσωπεύει τον έλεγχο Kolmogorov-Smirnov
 
 #έλεγχος κανονικότητας για τη μεταβλητή Points
 shapiro.test(Points)
 lillie.test(Points)
 
 #έλεγχος κανονικότητας για τη μεταβλητή Assists
 shapiro.test(Assists)
 lillie.test(Assists)
 
 #έλεγχος κανονικότητας για τη μεταβλητή Rebounds
 shapiro.test(Rebounds)
 lillie.test(Rebounds)
 
 #έλεγχος κανονικότητας για τη μεταβλητή Minutes
 shapiro.test(Minutes)
 lillie.test(Minutes)
 
 #έλεγχος κανονικότητας για τη μεταβλητή Height
 shapiro.test(Height)
 lillie.test(Height)
 
 #έλεγχος κανονικότητας για τη μεταβλητή Games
 shapiro.test(Games)
 lillie.test(Games)
 
 #έλεγχος κανονικότητας για τη μεταβλητή TPP
 shapiro.test(TPP)
 lillie.test(TPP)
 
 #έλεγχος κανονικότητας για τη μεταβλητή FT
 shapiro.test(FT)
 lillie.test(FT)
 
 #έλεγχος κανονικότητας για τη μεταβλητή Age
 shapiro.test(Age)
 lillie.test(Age)
 
 #έλεγχος κανονικότητας για τη μεταβλητή avg_game_time
 shapiro.test(avg_game_time)
 lillie.test(avg_game_time)
 
 #έλεγχος κανονικότητας για τη μεταβλητή PPM
 shapiro.test(PPM)
 lillie.test(PPM)
 
#με τις παρακάτω εντολές δημιουργούμε τα διαγράμματα πυκνότητας των μεταβλητών. Χρησιμοποιώντας την εντολή polygon(,col="lightblue"),
# γεμίζουμε το εσωτερικό της καμπύλης συχνότητας με μπλε χρώμα

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή Minutes
den<-density(Minutes)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή Points
den<-density(Points)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή Assists
den<-density(Assists)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή Rebounds
den<-density(Rebounds)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή TPP
den<-density(TPP)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή FT
den<-density(FT)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή PPM
den<-density(PPM)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή avg_game_time
den<-density(avg_game_time)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή Games
den<-density(Games)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή Height
den<-density(Height)
plot(den)
polygon(den, col = "lightblue")

#Δημιουργία καμπύλης πυκνότητας για τη μεταβλητή Age
den<-density(Age)
plot(den)
polygon(den, col = "lightblue")


#Σε αυτό το σημείο εκτελούμε τις εντολές προκειμένουμε να αναλύσουμε τις σχέσεις των μεταβλητών ανά δύο
 
#Αρχικά εκτελούμε τον έλεγχο συσχέτισης του Pearson

#Ελέγχουμε τη γραμμική συσχέτιση που έχει η μεταβλητή Points για καθέ μία ξεχωριστή μεταβλητή
cor(Age,Points)
cor(Height,Points)
cor(Minutes,Points)
cor(avg_game_time,Points)
cor(Assists,Points)
cor(Rebounds,Points)
cor(TPP,Points)
cor(FT,Points)
cor(Height,Points)
cor(over20, Points)

#Ελέγχουμε τη γραμμική συσχέτιση που έχει η μεταβλητή Age για καθέ μία ξεχωριστή μεταβλητή
cor(Age,Points)
cor(Height,Age)
cor(Minutes,Age)
cor(avg_game_time,Age)
cor(Assists,Age)
cor(Rebounds,Age)
cor(TPP,Age)
cor(FT,Age)
cor(Height,Age)
cor(over20,Age)

#Ελέγχουμε τη σχέση που έχουν οι μεταβλητές μεταξύ τους. Αν συγκρίνουμε ποσοτικές μεταβλητές χρησιμοποιούμε t.test(), 
#ενώ αν συγκρίνουμε κατηγορική με ποσοτική χρησιμοποιούμε τον 'ελεγχο X square
#t.test()
t.test(Age,Points)
t.test(Height,Points)
t.test(Assists,Points)
t.test(TPP,Points)
t.test(avg_game_time,Points)
t.test(Minutes,Points)
t.test(Games,Points)
t.test(over20,Points)
t.test(over20,TPP)
t.test(over20,avg_game_time)
t.test(over20,Age)
t.test(over20,PPM)

#X square
#Συγκρίνουμε τη μεταβλητή over20 με τα επίμερους τμήματα της μεταβλητής Age
chisq.test(table(Age4,over20)) #Συγκρίνουμε τη μεταβλητή over20 με τα επίμερους τμήματα της μεταβλητής Age

#οι εντολές που χρησιμοποιήσαμε για το 4ο μέρος της ανάλυσης μας(Ερμηνευτικά μοντέλα)

#ελέγχουμε τη σχέση που υπάρχει μεταξύ των  επιδόσεων των αθλητών στο σκοράρισμα ανά κατηγορία της κατηγορικής μεταβλητής over20
#δημιουργώντας 2 λίστες που περιέχουν τους πόντους των αθλητών με μέσο χρόνο συμμετοχής πάνω από 20 λεπτά και τους πόντους των αθλητών 
#με μέσο χρόνο συμμετοχής κάτω από 20 λεπτά
 po20<- numeric(0)
 pu20<- numeric(0)
 o<- 0
 u<- 0
 for (i in 1:105){
 if (over20[i]==1){
 o<-o+1
 po20[o]<- Points[i]
 }else{
 u<-u+1
 pu20[u]<- Points[i]
 }
 }
 
 wilcox.test(po20,pu20)
 
#ελέγχουμε τη σχέση που υπάρχει μεταξύ της ηλικίας των αθλητών  ανά κατηγορία της κατηγορικής μεταβλητής over20
#δημιουργώντας 2 λίστες που περιέχουν τις ηλικίες  των αθλητών με μέσο χρόνο συμμετοχής πάνω από 20 λεπτά και τις ηλικίες των αθλητών 
#με μέσο χρόνο συμμετοχής κάτω από 20 λεπτά
 ao20<- numeric(0)
 au20<- numeric(0)
 o<- 0
 u<-0
 for (i in 1:105){
 if (over20[i]==1){
 o<-o+1
 ao20[o]<- Age[i]
 }else{
 u<-u+1
 au20[u]<- Age[i]
 }
 }
 
 wilcox.test(ao20,au20)

#Εκτέλση γραμμικής παλλινδρόμησης με όλες τις μεταβήτές 
all<- lm(over20~Points+Assists+Rebounds+TPP+FT+Minutes+Games+avg_game_time+Height+Age+Age4)
summary(all)

#Εκτέλεση γενικευμένης γραμμικής παλλινδρόμησης
g_all<- glm(over20~Points+Assists+Rebounds+TPP+FT+Minutes+Games+avg_game_time+Height+Age+Age4)
summary(g_all)

#Εκτέλση γραμμικής παλλινδρόμησης με όλες τις μεταβήτές πλην της σταθερά 
model<- lm(over20~Points+Assists+Rebounds+TPP+FT+Minutes+Games+avg_game_time+Height+Age+Age4+0)
summary(model)

#Εκτέλεση γενικευμένης γραμμικής παλλινδρόμησης με όλες τις μεταβλητές πλην της σταθερά
g_model<- glm(over20~Points+Assists+Rebounds+TPP+FT+Minutes+Games+avg_game_time+Height+Age+Age4)
summary(g_model)

#Εκτέλση γραμμικής παλλινδρόμησης στο τελικό μοντέλο
final_model<- lm(over20~Points+TPP+avg_game_time+Height)
summary(final_model)

#Εκτέλεση γενικευμένης γραμμικής παλλινδρόμησης στο τελικό μοντέλο
g_final_model<- glm(over20~Points+TPP+avg_game_time+Height)
summary(g_final_model)

#υπολογίζοντας τη συνδεσιμότητα των μεταβλητών του τελικού μοντέλου
vif(final_model)

#οι εντολές που εκτελέσαμε προκειμένου να εξάγουμε τελικό συμπέρασμα
#δημιουργούμε 5 λίστες με τις κατηγορίες των χαρακτηριστικών που μας ενδιαφέρουν 
paikths<- character(0)
pontoi<- numeric(0)
tp<- numeric(0)
xronos<- numeric(0)
ipsos<- numeric(0)
j<-0
#Αποθηκεύουμε εκείνους τους παίκτες που έχουν μέσο χρόνο συμμετοχής πάνω από 20 λεπτά και μέσο όρο πόντων ανά αγώνα πάνω από το γενικό μέσο όρο πόντων
for (i in 1:105){
if (Points[i]>mean(Points)){
j<- j+1
paikths[j]<- Player[i]
pontoi[j]<- Points[i]
tp[j]<- TPP[i]
xronos[j]<- avg_game_time[i]
ipsos[j]<- Height[i]
}
}
#Δημιουργούμε το data frame
dataframe<- data.frame(paikths, pontoi, tp, xronos,ipsos)
#Με τις παρακάτω εντολές το ταξινομούμε με βάση τις επιμέρους κατηγορίες και βλέπουμε τα σχετικά αποτελέσματα
dataframe<- arrange(dataframe, desc(pontoi))
dataframe<- arrange(dataframe, desc(tp))
dataframe<- arrange(dataframe, desc(xronos))
dataframe<- arrange(dataframe, desc(ipsos))

 
#Εντολές για την υλοποιήση του παραρτήματος
 
 #Δημιουργώ qqplot για κάθε μεταβλητή με την ευθεία της κανονικής της κατνομής
qqnorm(Points)
qqline(Points)

qqnorm(Assists)
qqline(Assists)

qqnorm(Rebounds)
qqline(Rebounds)

qqnorm(TPP)
qqline(TPP)

qqnorm(FT)
qqline(FT)

qqnorm(Minutes)
qqline(Minutes)

qqnorm(Games)
qqline(Games)


#Δημιουργία barplots για σύγκριση μεταβλητών
barplot(Points, main = "Points'Barplot")
barplot(Age, main = "Age's Barplot")
barplot(TPP, main = "Points'Barplot")
barplot(FT, main = "Points'Barplot")
barplot(avg_game_time,Points)

 
#Δημιουργία boxplots για σύγκριση μεταβλητών
boxplot(Age,Points)
boxplot(Age,Points, title="Age-Points")
boxplot(Age,Points, main="Age-Points")
boxplot(over20,Points, main="over20-Points")
boxplot(avg_game_time,Points, main="Average Game Time-Points")
boxplot(Age,Points, main="Age-Points")
boxplot(avg_game_time,Points, main="Average Game Time-Points")
boxplot(Games,Points, main="Games-Points")
boxplot(Height,Points, main="Height-Points")
boxplot(Age,TPP, main="Age-2Point%")
boxplot(Age,avg_game_time, main="Age-Average Game Time")
boxplot(Age,FT, main="Age-Free Throw")
boxplot(Age,Games, main="Age-Games")
boxplot(Age,FT, main="Age-Free Throw")

#Δημιουργία scatterplots για σύγκριση μεταβλητών
scatterplot(Age4,TPP)
scatterplot(Age4,Games)
scatterplot(Age4,Assists)
scatterplot(Age4,Rebounds)
scatterplot(Age4,FT)
scatterplot(Age4,over20)
scatterplot(Age4,Points)
scatterplot(Age4,avg_game_time)

 
#Plots για τους ελέγχους υποθέσεων των μοντέλων που δημιουργήσαμε 
 
residuals <- rstandard(all)
qqplot(qnorm(ppoints(length(residuals))), residuals, xlab = "Theoretical Quantities", ylab = "Standardized Residuals")
qqline(residuals, col = "red")

residuals <- residuals(all)
leverage <- hatvalues(all)
plot(leverage, residuals, xlab = "Leverage", ylab = "Residuals", main = "Residuals vs Leverage")
abline(h = 0, lty = 2)
abline(h = mean(residuals), col = "red") 
abline(h = c(mean(residuals) - sd(residuals), mean(residuals) + sd(residuals)), col = "blue") 

residuals <- residuals(all)
fitted.values <- fitted(all)
plot(fitted.values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")
lines(lowess(fitted.values, residuals), col = "red")


residuals <- rstandard(model)
qqplot(qnorm(ppoints(length(residuals))), residuals, xlab = "Theoretical Quantities", ylab = "Standardized Residuals")
qqline(residuals, col = "red")

residuals <- residuals(model)
leverage <- hatvalues(model)
plot(leverage, residuals, xlab = "Leverage", ylab = "Residuals", main = "Residuals vs Leverage")
abline(h = 0, lty = 2)
abline(h = mean(residuals), col = "red") 
abline(h = c(mean(residuals) - sd(residuals), mean(residuals) + sd(residuals)), col = "blue") 

residuals <- residuals(model)
fitted.values <- fitted(model)
plot(fitted.values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")
lines(lowess(fitted.values, residuals), col = "red")


 
residuals <- rstandard(final_model)
qqplot(qnorm(ppoints(length(residuals))), residuals, xlab = "Theoretical Quantities", ylab = "Standardized Residuals")
qqline(residuals, col = "red")

residuals <- residuals(final_model)
leverage <- hatvalues(final_model)
plot(leverage, residuals, xlab = "Leverage", ylab = "Residuals", main = "Residuals vs Leverage")
abline(h = 0, lty = 2)
abline(h = mean(residuals), col = "red") 
abline(h = c(mean(residuals) - sd(residuals), mean(residuals) + sd(residuals)), col = "blue") 

residuals <- residuals(final_model)
fitted.values <- fitted(final_model)
plot(fitted.values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")
lines(lowess(fitted.values, residuals), col = "red")
 
 
 
