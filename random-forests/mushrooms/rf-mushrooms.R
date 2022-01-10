mushrooms <- read.csv("mushrooms.csv", sep=',', header=TRUE)
mushrooms$class[mushrooms$class == 'e'] <- 'edible'
mushrooms$class[mushrooms$class == 'p'] <- 'poisonous'
for (i in 1:ncol(mushrooms)) {
  if (is.character(mushrooms[,i])) mushrooms[,i] <- as.factor(mushrooms[,i])
}
#mushrooms[,-1] <- as.factor(mushrooms[,-1])

(freq.table <- table(mushrooms$class))
(prop.table <- prop.table(freq.table))

# create train and test
n=dim(mushrooms)[1]
train.rows=sample(n, round(.8*n))
m.train=mushrooms[train.rows,]
m.test=mushrooms[-train.rows,]

# confirm similar
table(m.train$class)
table(m.test$class)

# Random Forest
# install.packages("randomForest")
library(randomForest)

# find out the column with the response variable
names(m.train)
out.rf=randomForest(x=m.train[,-1],y=m.train$class,
                        xtest=m.test[,-1],ytest=m.test$class,
                        replace=TRUE,
                        keep.forest=TRUE,
                        ntree=50,
                        mtry=5,
                        nodesize=25)

out.rf

# Which variables are most important in determining edibility (class)?
varImpPlot(out.rf, main="Explanatory Variables")
# Odor and spore print color

# demonstrate a new prediction
new.obs <- as.factor(c(cap.shape='s', cap.surface='y', cap.color='w', bruises='t', 
                       odor='a', gill.attachment='f', gill.spacing='c', gill.size='b', 
                       gill.color='k', stalk.shape='e', stalk.root='c', stalk.surface.above.ring='s', 
                       stalk.surface.below.ring='s', stalk.color.above.ring='w', stalk.color.below.ring='w', 
                       veil.type='p', veil.color='w', ring.number='o', ring.type='p', spore.print.color='k', 
                       population='s', habitat='g'))
predict(out.rf, newdata=new.obs, type='prob')
# 56% probability of being edible

