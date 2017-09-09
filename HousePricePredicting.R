housing.data = read.csv("train.csv", stringsAsFactors = FALSE)

model_var <- c('Id', 'SalePrice', 
               'OverallQual','OverallCond','YearBuilt', 'ExterQual','ExterCond',
               'TotalBsmtSF','HeatingQC', 
               'CentralAir','GrLivArea','BedroomAbvGr','KitchenAbvGr',
               'TotRmsAbvGrd','Fireplaces',
               'GarageArea','OpenPorchSF','PoolArea',
               'YrSold')

train <- housing.data[,model_var]
View(train)

### Splitting the Train Data into 80 - 20 ###
train.index <- sample(c(1:dim(train)[1]), dim(train)[1]*0.8)
model_lin_train = train[train.index,]
model_lin_valid <- train[-train.index,]

## Creating the Model 
housing.model = lm(formula  = SalePrice ~ OverallQual  + OverallCond + YearBuilt + ExterQual 
                   + ExterCond + TotalBsmtSF 
                   + HeatingQC + CentralAir + GrLivArea + BedroomAbvGr + 
                     KitchenAbvGr + TotRmsAbvGrd + Fireplaces 
                   + GarageArea + OpenPorchSF + PoolArea + YrSold
                   , data = model_lin_train)
summary(housing.model)

pred = model_lin_valid[,c("Id","YrSold")]
View(pred)

### Predicting the Test Data 
pred$Prediction = predict(object = housing.model, newdata = model_lin_valid)

## View the Predicted House Price Values
View(pred)

