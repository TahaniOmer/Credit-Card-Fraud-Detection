
# Inspired from Kaggle Dataset - https://www.kaggle.com/mlg-ulb/creditcardfraud

# Data Exploration

credit = read.csv("creditcard.csv",header = T,sep = ",")
head(credit)
summary(credit)
str(credit) 

sum(is.na(credit))  #Check for missing values

# Data pre-processing

#our model is invariant to scale, built in featur selection and discretizations
attach(credit)
table(Class)  #check for imbalance

# Decision Tree

library(tree) # to construct classification tree
head( Class )
fraud = ifelse( Class<=0.5,"No","Yes" )
credit = data.frame(credit,fraud)  #to merge fraud with the rest of the credit data.
head(credit)
set.seed(2)
train =  sample(nrow(credit), nrow(credit) * 0.80) 
#to split the observations into a training and a testing set

tree.credit = tree( fraud~.- Class ,credit, subset = train)
summary(tree.credit)
tree.credit
plot(tree.credit)  #to display the tree structure
text(tree.credit,pretty = 0)  #to display the node labels
tree.pred = predict(tree.credit,credit[-train,],type = "class")
#type="class" instructs R to return the actual class prediction
with(credit[-train,],table(tree.pred,fraud))

#Pruning

cv.credit = cv.tree(tree.credit,FUN = prune.misclass)
cv.credit
#plot the error rate as a function of both size and k
plot(cv.credit)
prune.credit = prune.misclass(tree.credit,best =5)
plot(prune.credit)
text(prune.credit,pretty = 0)
prune.pred = predict(prune.credit,credit[-train,],type = "class")
with(credit[-train,],table(prune.pred,fraud))

library(caret) # for accuracy measures
fraud = as.factor(fraud)
confusionMatrix(tree.pred,fraud[-train])














