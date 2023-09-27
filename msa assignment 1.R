##UPLOADING DATA:-
data=read.csv(file.choose(),header=T)
data

##Change the decimal point from , (comma) to . (dot)
#library(stringr)
#str_replace_all(data$PT08.S2.NMHC., ",", ".")
#but commas are already changed

##Change -200 to missing value
data[data==-200.0]=200
data[data==-200]=NA
data

##Remove column numbers 1, 2 and 5. 
data2 <- data[ -c(1:2,5) ]
data2

##Compute the correlation matrix in two ways using the options use = "complete.obs" and use="pair".

#Using "complete.obs" function
crm1 <- cor(data2, use="complete.obs", method="pearson") 
crm1

#using "pair" function
crm2 <- cor(data2, use="pair", method="pearson") 
crm2


##Find the eigenvalues and eigenvectors of the correlation matrix in each case.

#for complete method
c=eigen(crm1)

eval_c= c$values
evec_c= c$vectors

eval_c
evec_c

#for pair method
p=eigen(crm2)

eval_p= p$values
evec_p= p$vectors

eval_p
evec_p

##In each case, identify eigenvalues greater than unity and corresponding eigenvectors.

#for complete method
eval_1c = eval_c[c(1,2,3)] #eigenvalues
eval_1c

evec_1c = evec_c[,1:3]  #eigenvectors
evec_1c

#for pair method
eval_1p = eval_p[c(1,2,3)] #eigenvalues
eval_1p

evec_1p = evec_p[,1:3] #eigenvectors
evec_1p

##Compute principal component scores in each case

pca1=as.matrix(data2)%*%evec_1c
pca2=as.matrix(data2)%*%evec_1p

pca1
pca2
