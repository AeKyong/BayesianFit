correlation = .7
nAttributes = 5
nSample = 10000
propMasters = rep(.5, nAttributes) # propMaster: means difficulty, not probability of masters for an attribute
cutPoints = qnorm(propMasters)

if (!require(mvtnorm)) install.packages("mvtnorm")
library(mvtnorm)

# build tetrachoric correlation matrix
tetrachoricCorrelationMatrix = matrix(
    1,
    nrow = nAttributes,
    ncol = nAttributes
)

for (a1 in 1:(nAttributes-1)){
    for (a2 in (a1+1):nAttributes){
        tetrachoricCorrelationMatrix[a1,a2] = correlation
        tetrachoricCorrelationMatrix[a2,a1] = correlation
    }
}

underlying = mvtnorm::rmvnorm(
    n = nSample,
    mean = rep(0, nAttributes),
    sigma = tetrachoricCorrelationMatrix
)

attributes = underlying
# categorize attributes
for (attribute in 1:nAttributes){
    attributes[which(underlying[,attribute] < cutPoints[attribute]), attribute] = 0
    attributes[which(underlying[,attribute] >= cutPoints[attribute]), attribute] = 1
}

library(psych)
tetrachoric(attributes)


bin2dec = function(binary_vector, nattributes, basevector){
  dec = 0
  for (i in nattributes:1){
    dec = dec + binary_vector[i]*(basevector[i]^(nattributes-i));
  }
  return(dec)
}

class = NULL
person = 1
for (person in 1:nrow(attributes)){
    class = c(class, bin2dec(attributes[person,], nAttributes, rep(2, nAttributes))+1)
}

table(class)/nSample

a = table(class)/nSample
write.csv(a, "jointprob.csv")
