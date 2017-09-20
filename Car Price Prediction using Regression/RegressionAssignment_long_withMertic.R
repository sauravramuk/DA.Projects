library("ggplot2")
library("tidyr")
library("stats")
library("car")
library("MASS")
library("dplyr")


cars1 <- read.csv("carprice_assignment.csv", stringsAsFactors = F)
cars <- cars1[,]


#Proper formatting of the columnn names


names(x = cars) <- c("CarId","Symboling","CarName","FuelType","Aspiration","DoorNumber","CarBody","DriveWheel","EngineLocation","WheelBase","CarLength","CarWidth","CarHeight","CurbWeight","EngineType","NumberOfCylinder","EngineSize","FuelSystem","BoreRatio","Stroke","CompressionRatio","HorsePower","PeakRPM","CityMPG","HighwayMPG","Price")



###Data Understanding###
#Dataset
nrow(cars) #205
nrow(unique(cars))#205 So no duplicate rows

#CarId
length(unique(cars$CarId))
which(is.na(cars$CarId))#no NA values
#Since this coulmn is only a unique identifier, it can be dropped as it will interfere with the modelling
cars <- cars[,-1]

#Symboling
unique(cars$Symboling) 
#none has -3 rating, this is categorical variable
#since lower is better, assigning values so that a more reilable car has a higher value.
#values will range from 0 to 6(eg: -3 will have 0 value)
#symbl <- model.matrix(~Symboling, data = cars)
cars$Symboling <- sapply(X = cars$Symboling, FUN = function(x) return((x-3)*-1) ,simplify = T )


#CarName
#Since we need to use only the car company name We will extract the company name
length(unique(cars$CarName))
cars <- separate(data = cars, col = CarName, into = c('CarCompanyName', 'ModelName', 'ModelNumber'), sep = " ", remove = F, extra = "drop", )

length(which(is.na(cars$CarCompanyName)))# Every row has the company name
#removing the other generated rows
cars <- cars[,c(-2,-4,-5)]

#column: FuelType
unique(cars$FuelType)# two types
#they are categorical variable so, converting them into dummy values
x <- model.matrix(~FuelType, data = cars)
cars <- cbind(cars[,-3],FuelTypeGas=x[,-1])


#column:Aspiration
unique(cars$Aspiration)
#categorical will convert to dummy variables
x <- model.matrix(~Aspiration, data = cars)
cars <- cbind(cars[,-3],AspirationTurbo=x[,-1])

#Column:DoorNumber
unique(cars$DoorNumber)
#Categorical variable with two values will assign dummy variable
x <- model.matrix(~DoorNumber, data = cars)
cars <- cbind(cars[,-3],NumberOfDoor.Two=x[,-1])

#Column:CarBody
unique(cars$CarBody)
#Categorical variable with two values will assign dummy variable
x <- model.matrix(~CarBody, data = cars)
cars <- cbind(cars[,-3],CarBody.HardTop = x[,2], CarBody.Hatchback =x[,3], CarBody.Sedan =x[,4], CarBody.Wagon =x[,5])


#Column DriveWheel
unique(cars$DriveWheel)
#Categorical variable with two values will assign dummy variable
x <- model.matrix(~DriveWheel, data = cars)
cars <- cbind(cars[,-3], DriveWheel.FWD = x[,2], DriveWheel.RWD =x[,3])

#Column:EngineLocation
unique(cars$EngineLocation)
#Categorical variable with two values will assign dummy variable
x <- model.matrix(~EngineLocation, data = cars)
cars <- cbind(cars[,-3], EngineLocation.Rear = x[,2])

#Column:WHeelBase
length(which(is.na(cars$WheelBase)))
summary(cars$WheelBase)

#Column:CarLength
length(which(is.na(cars$CarLength)))
summary(cars$CarLength)

#Column:CarWidth
length(which(is.na(cars$CarWidth)))
summary(cars$CarWidth)

#Column:CarHeight
length(which(is.na(cars$CarHeight)))
summary(cars$CarHeight)

#Column:CurbWeight
length(which(is.na(cars$CurbWeight)))
summary(cars$CurbWeight)

#Column:EngineType
unique(cars$EngineType)
#Categorical variable with two values will assign dummy variable
x <- model.matrix(~EngineType, data = cars)
cars <- cbind(cars[,-8], EngineType.DOHCV = x[,2], EngineType.L = x[,3], EngineType.OHC = x[,4], EngineType.OHCF = x[,5], EngineType.OHCV = x[,6], EngineType.Rotor = x[,7])

#Column: Number of cylinder
unique(cars$NumberOfCylinder)
#Categorical variable with two values will assign dummy variable
#Will convert the number spelling to digit (eg: two = 2)

cars$NumberOfCylinder <-  factor(cars$NumberOfCylinder)
#changed level values to numeric
levels(cars$NumberOfCylinder) <- c(8,5,4,6,3,12,2)
#FActored to vector
cars$NumberOfCylinder <- as.numeric(levels(cars$NumberOfCylinder))[cars$NumberOfCylinder]


#Column:EngineSize
length(which(is.na(cars$EngineSize)))
summary(cars$EngineSize)


#Column:FuelSystem
unique(cars$FuelSystem)
#Categorical variable with two values will assign dummy variable
x <- model.matrix(~FuelSystem, data = cars)
cars <- cbind(cars[,-10], FuelSystem.2BBL = x[,2], FuelSystem.4BBL = x[,3], FuelSystem.IDI = x[,4], FuelSystem.MFI = x[,5], FuelSystem.MPFI = x[,6], FuelSystem.SPDI = x[,7], FuelSystem.SPFI = x[,8])


#Column:BoreRatio
length(which(is.na(cars$BoreRatio)))
summary(cars$BoreRatio)

#Column:CompressionRatio
length(which(is.na(cars$CompressionRatio)))
summary(cars$CompressionRatio)

#Column:HorsePower
length(which(is.na(cars$HorsePower)))
summary(cars$HorsePower)

#Column:PeakRPM
length(which(is.na(cars$PeakRPM)))
summary(cars$PeakRPM)

#Column:CityMPG
length(which(is.na(cars$CityMPG)))
summary(cars$CityMPG)

#Column:HighwayMPG
length(which(is.na(cars$HighwayMPG)))
summary(cars$HighwayMPG)

#Column:Stroke
length(which(is.na(cars$Stroke)))
summary(cars$Stroke)


#Column: CarCompanyNames 
unique(cars$CarCompanyName)
#We see that  few of the cars have been mispelt causing difference when they are the same
#maxda is mazda
cars$CarCompanyName[cars$CarCompanyName =="maxda"] <- "mazda"

#Nissan is nissan
cars$CarCompanyName[cars$CarCompanyName =="Nissan"] <- "nissan"

#porcshce is porsche
cars$CarCompanyName[cars$CarCompanyName =="porcshce"] <- "porsche"

#toyouta is toyota
cars$CarCompanyName[cars$CarCompanyName =="toyouta"] <- "toyota"

#vokswagen is volkswagen
cars$CarCompanyName[cars$CarCompanyName =="vokswagen"] <- "volkswagen"

#vw is volkswagen
cars$CarCompanyName[cars$CarCompanyName =="vw"] <- "volkswagen"

#Categorical variable with two values will assign dummy variable
#x <- model.matrix(~FuelSystem, data = cars)
#cars <- cbind(cars[,-10], FuelSystem.2BBL = x[,2], FuelSystem.4BBL = x[,3], FuelSystem.IDI = x[,4], FuelSystem.MFI = x[,5], FuelSystem.MPFI = x[,6], FuelSystem.SPDI = x[,7], FuelSystem.SPFI = x[,8])
# 
# plot.carvsprice <-  ggplot(data = cars, aes(x = CarCompanyName, y = Price)) + geom_point()
# 
# ggplot(data = cars, aes(x = Price)) + geom_histogram(binwidth = 4000, aes(fill = factor(CarCompanyName)))
# 
#to prevent extra step if some error happens :)
cars2 <- cars[,]
x <- model.matrix(~CarCompanyName, data = cars2)
x <- as.data.frame(x)
cars2 <- cbind(cars2[,-2], x[-1])

#Metric: CarSize, Defined using the classification table: US Insurance Institute for Highway 
#Safety | Highway Loss Data Institute 'Guide to car size groups' (includes minivans). Please refer
# the LInk:https://en.wikipedia.org/wiki/Car_classification
#based upon the classification criteria, created a function:
#IMPORTANT:the classification is done on the assumption that the lengths are in inches and
#weigh is in pounds
#Size definition:
# mini = 1
# small = 2
# midsize = 3
# large = 4
# v.large = 5

#car.area>=70 &&
#curb.weight>=2000 &&
ClassifyMyCar <- function(car.row){
  car.width <- car.row[1]
  car.length <- car.row[2]
  curb.weight <- car.row[3]
  #convert lengths to feet
  car.weight.ft <- car.width/12
  car.length.ft <- car.length/12
  car.area <- car.length.ft*car.weight.ft
  
  if(car.area<81){
    if( curb.weight <=2500){
      return(1)
    }else if(curb.weight>2500 && curb.weight <=3000){
      return(2)
    }else if(curb.weight>3000 && curb.weight <=3500){
      return(2)
    }else if(curb.weight>3500 && curb.weight <=4000){
      return(2)
    }else if(curb.weight>4000){
      return(3)
    }else
      return("-")
    
  }else if(car.area<91 && car.area>=81){
    
    if( curb.weight <=2500){
      return(2)
    }else if(curb.weight>2500 && curb.weight <=3000){
      return(2)
    }else if(curb.weight>3000 && curb.weight <=3500){
      return(3)
    }else if(curb.weight>3500 && curb.weight <=4000){
      return(3)
    }else if(curb.weight>4000){
      return(3)
    }else
      return("-")
    
  }else if(car.area<101 && car.area>91){
    
    if( curb.weight <=2500){
      return(2)
    }else if(curb.weight>2500 && curb.weight <=3000){
      return(3)
    }else if(curb.weight>3000 && curb.weight <=3500){
      return(3)
    }else if(curb.weight>3500 && curb.weight <=4000){
      return(4)
    }else if(curb.weight>4000){
      return(4)
    }else
      return("-")
    
  }else if(car.area<=110 && car.area>101){
    
    if( curb.weight <=2500){
      return(2)
    }else if(curb.weight>2500 && curb.weight <=3000){
      return(3)
    }else if(curb.weight>3000 && curb.weight <=3500){
      return(4)
    }else if(curb.weight>3500 && curb.weight <=4000){
      return(4)
    }else if(curb.weight>4000){
      return(5)
    }else
      return("-")
    
  }else if( car.area>110){
    
    if( curb.weight <=2500){
      return(3)
    }else if(curb.weight>2500 && curb.weight <=3000){
      return(3)
    }else if(curb.weight>3000 && curb.weight <=3500){
      return(4)
    }else if(curb.weight>3500 && curb.weight <=4000){
      return(5)
    }else if(curb.weight>4000){
      return(5)
    }else
      return("-")
  }
}

#ClassifyMyCar(cars2[205,c(3,4,6)])

x <- as.numeric(unlist( apply(X=cars2[,c(3,4,6)], MARGIN = 1,FUN = ClassifyMyCar )))
cars2 <- cbind(cars2, CarSize=x)


#seed
set.seed(100)

indices <- sample(1:nrow(cars2), 0.7*nrow(cars2))

#trainData
cars2.Train <- cars2[indices,]

#testData
cars2.Test <- cars2[-indices,]

#model 1
model1 <- lm(formula = Price~., data = cars2.Train)

summary(model1)

#too many variables... using stepAIC to remove a few
AICValues <- stepAIC(model1, direction = "both")

#training data from AIC model
cars2.Train <- AICValues$model

#model2
model2 <- lm(formula = Price~., data = cars2.Train)
summary(model2)
model.vif <- vif(model2)

#excluding horsepower from the model due to insignificance and high VIF
model3 <- lm(Price~.-HorsePower, cars2.Train)
summary(model3)
model.VIF <- vif(model3)

#excluding CarLength from the model due to insignificance and high VIF
model4 <- lm(Price~.-HorsePower-CarLength, cars2.Train)
summary(model4)
model.VIF <- vif(model4)


#excluding BoreRatio from the model due to insignificance and high VIF
model5 <- lm(Price~.-HorsePower-CarLength-BoreRatio, cars2.Train)
summary(model5)
model.VIF <- vif(model5)


#excluding CarCompanyNamechevrolet from the model due to insignificance 
model6 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet, cars2.Train)
summary(model6)
model.VIF <- vif(model6)

#excluding FuelSystem.SPDI from the model due to v.low significance 
model7 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI, cars2.Train)
summary(model7)
model.VIF <- vif(model7)


#excluding CarCompanyNamesaab from the model due to v.low significance 
model8 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
             , cars2.Train)
summary(model8)
model.VIF <- vif(model8)



#excluding EngineType.OHC from the model due to insignificance 
model9 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
             -EngineType.OHC, cars2.Train)
summary(model9)
model.VIF <- vif(model9)


#excluding EngineType.L from the model due to insignificance 
model10 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
             -EngineType.OHC-EngineType.L, cars2.Train)
summary(model10)
model.VIF <- vif(model10)

#excluding CurbWeight from the model due to low significance and high VIF 
model11 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight, cars2.Train)
summary(model11)
model.VIF <- vif(model11)

#excluding CityMPG from the model due to insignificance and high VIF 
model12 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG, cars2.Train)
summary(model12)
model.VIF <- vif(model12)


#excluding CarBody.Wagon  from the model due to low significance and high VIF 
model13 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon , cars2.Train)
summary(model13)
model.VIF <- vif(model13)


#excluding CarBody.Sedan  from the model due to low significance and high VIF 
model14 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan , cars2.Train)
summary(model14)
model.VIF <- vif(model14)



#excluding CarBody.HardTop  from the model due to insignificance and high VIF 
model15 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop , cars2.Train)
summary(model15)
model.VIF <- vif(model15)


  
#excluding CarBody.Hatchback  from the model due to insignificance and high VIF 
model16 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              , cars2.Train)
summary(model16)
model.VIF <- vif(model16)


#excluding CarCompanyNamevolkswagen  from the model due to insignificance.
model17 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen, cars2.Train)
summary(model17)
model.VIF <- vif(model17)


#excluding FuelSystem.MPFI  from the model due to low significance and high VIF.
model18 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI, cars2.Train)
summary(model18)
model.VIF <- vif(model18)


#excluding FuelSystem.2BBL  from the model due to insignificance.
model19 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL, cars2.Train)
summary(model19)
model.VIF <- vif(model19)

#excluding CarCompanyNamerenault  from the model due to insignificance
model20 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL-CarCompanyNamerenault, cars2.Train)
summary(model20)
model.VIF <- vif(model20)

#we seems to have a fair model. try to predict
model.predicted <- predict(object = model20, newdata = cars2.Test)
Pre.test.corrl <- cor(model.predicted, cars2.Test$Price)
Pre.test.corrl^2
#seems we can improve this further



#excluding DriveWheel.RWD  from the model due to low significance and higher VIF
model21 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL-CarCompanyNamerenault-DriveWheel.RWD, cars2.Train)
summary(model21)
model.VIF <- vif(model21)

#excluding CarCompanyNamenissan  from the model due to low significance.
model22 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL-CarCompanyNamerenault-DriveWheel.RWD-CarCompanyNamenissan, cars2.Train)
summary(model22)
model.VIF <- vif(model22)



#excluding CarCompanyNamemazda  from the model due to low significance.
model23 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL-CarCompanyNamerenault-DriveWheel.RWD-CarCompanyNamenissan
              -CarCompanyNamemazda, cars2.Train)
summary(model23)
model.VIF <- vif(model23)

#excluding CarCompanyNametoyota  from the model due to low significance.
model24 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL-CarCompanyNamerenault-DriveWheel.RWD-CarCompanyNamenissan
              -CarCompanyNamemazda-CarCompanyNametoyota, cars2.Train)
summary(model24)
model.VIF <- vif(model24)


#excluding CarCompanyNamedodge  from the model due to low significance.
model25 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL-CarCompanyNamerenault-DriveWheel.RWD-CarCompanyNamenissan
              -CarCompanyNamemazda-CarCompanyNametoyota-CarCompanyNamedodge, cars2.Train)
summary(model25)
model.VIF <- vif(model25)

#excluding CarCompanyNameplymouth  from the model due to insignificance.
model26 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL-CarCompanyNamerenault-DriveWheel.RWD-CarCompanyNamenissan
              -CarCompanyNamemazda-CarCompanyNametoyota-CarCompanyNamedodge-CarCompanyNameplymouth, cars2.Train)
summary(model26)
model.VIF <- vif(model26)

#excluding EngineType.DOHCV   from the model due to low significance.
model27 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL-CarCompanyNamerenault-DriveWheel.RWD-CarCompanyNamenissan
              -CarCompanyNamemazda-CarCompanyNametoyota-CarCompanyNamedodge-CarCompanyNameplymouth-EngineType.DOHCV , cars2.Train)
summary(model27)
model.VIF <- vif(model27)

#we seems to have a model with highsignificans and mostly independent variables
#trying to predict and test the model
model.predicted <- predict(object = model27, newdata = cars2.Test)
Pre.test.corrl <- cor(model.predicted, cars2.Test$Price)
Pre.test.corrl^2

#excluding CarCompanyNamemitsubishi   from the model due to low significance.
model28 <- lm(Price~.-HorsePower-CarLength-BoreRatio-CarCompanyNamechevrolet-FuelSystem.SPDI-CarCompanyNamesaab
              -EngineType.OHC-EngineType.L-CurbWeight-CityMPG-CarBody.Wagon-CarBody.Sedan-CarBody.HardTop-CarBody.Hatchback
              -CarCompanyNamevolkswagen-FuelSystem.MPFI-FuelSystem.2BBL-CarCompanyNamerenault-DriveWheel.RWD-CarCompanyNamenissan
              -CarCompanyNamemazda-CarCompanyNametoyota-CarCompanyNamedodge-CarCompanyNameplymouth-EngineType.DOHCV-CarCompanyNamemitsubishi
              , cars2.Train)
summary(model28)
model.VIF <- vif(model28)

#trying to predict and test the model
model.predicted <- predict(object = model28, newdata = cars2.Test)
Pre.test.corrl <- cor(model.predicted, cars2.Test$Price)
Pre.test.corrl^2

####Hence the model that we come to is:

#Price = B0 + B1*X1 + B2*X2 + B3*X3 + B4*X4 + B5*X5 + B6*X6 + B7*X7 + B8*X8 + B9*X9 + B10*X10 +B11*X11

# B1=SLOPE OF CarWidth(X1) = 995.746        
# B2=SLOPE OF EngineSize(X2) = 101.708            
# B3=SLOPE OF Stroke(X3) = -4509.658        
# B4=SLOPE OF PeakRPM(X4) = 1.706            
# B5=SLOPE OF AspirationTurbo(X5) = 2306.854        
# B6=SLOPE OF EngineLocation.Rear(X6) = 17245.790      
# B7=SLOPE OF EngineType.OHCF(X7) = -3554.958       
# B8=SLOPE OF EngineType.Rotor(X8) = 4652.910       
# B9=SLOPE OF CarCompanyNamebmw(X9) = 8032.905      
# B10=SLOPE OF CarCompanyNamebuick(X10) = 7361.208    
# B11=SLOPE OF CarCompanyNamejaguar(X11) = 9661.917   
# B0=INTERCEPT = -60603.132   

####further analysis

#variable 1 : carwidth:
#When car width increases, so does the length, this increases the cost of production as it translates to more material use, 
#as we see from the plot below the car length is positively correlated with it.
cor(cars1$carwidth, cars1$carlength)
plot.carwidthvslength <- ggplot(data = cars1, aes(x = carwidth, y = carlength)) + geom_point(aes(), size=4)
plot.carwidthvslength


#variable 2 :EngineSize:
#Higher engine size leads to higher power, are heavier. To handle them we require higher performance parts like
#better breaking systems, suspension, tyers etc. increasing the costs further

#engine to horsepower relationship:
plot.enginevshorsepower <- ggplot(data = cars1, aes(x = enginesize, y = horsepower))+geom_point()
plot.enginevshorsepower

#Engine curb weight and price relationship plot
plot.engineVscurbweightVsPrice <- ggplot(data = cars1, aes(x = enginesize, y = curbweight)) + geom_point(aes(col = price), size = 3) +geom_smooth()
plot.engineVscurbweightVsPrice


#Variable3: Stroke: This is the stroke length of the engine

#variable4:PeakRPM is engine rev speed
plot.rpmvsprice <- cars1 %>% 
                    group_by(fueltype, peakrpm, price) %>% 
                      summarise() %>% 
                        ggplot(aes(x = peakrpm, y = price))+geom_point(aes(col=fueltype))


#variable 5 : AspirationTurbo
#A turbo charged engined will be pricy because it requires a turbocharger, produces more power under higher pressures 
#so it requires better stronger parts & equiptments

#variable 6:EngineLocation.Rear
#A rear engine raises significant costs due to its 
#distance from the driver: this leads to longer wiring & pipings as engine is much further from the driver
#longer distance between cooling system that is infront and engine that is rear
#rear engine uusually occurs in a performance car.


#variable 7-9:Car brands BMW, BUICK, Jaguar
#these are luxury cars they generally have premium parts, equiptments, and other 
#luxury items (eg: Plush leather seats, high end stereo)

#plot to show premium branded cars are bigger(Porsche is an exception!)
plot.PremiumCarsAreBigger <- separate(data = cars1[cars1$price>=25000,], 
                                       col = CarName,
                                       into = c('CarCompanyName'),
                                       sep = " ", remove = F,
                                       extra = "drop" ) %>%
                              ggplot(aes(x = carwidth, y = carlength)) + geom_point(aes(col = CarCompanyName),size = 3)
plot(model28)
