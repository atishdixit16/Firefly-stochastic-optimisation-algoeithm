randomTrials <- function(count = 100, varCount = 2, range = matrix(c(-5,-5,5,5),2,2)) {
        trialStack <- NULL
        for (i in 1:count) {
                trial <- NULL
                for (j in 1:varCount)
                        trial <- c( trial, runif(1,range[j,1],range[j,2]) )
                trialStack <- rbind(trialStack,trial)
        }
        trialStack
}

inputFunction <- function(values) {
       - ( -20*exp(-0.2*sqrt(0.5*(values[1]^2 + values[2]^2))) - exp(0.5*(cos(2*pi*values[1]) + cos(2*pi*values[2])) ) + exp(1) + 20 )
}

euclDist <- function(x, y) {
	sqrt(sum((x - y)**2))
}

lightIntensity <- function(trialStack) {
	light <- NULL
	rows <- nrow(trialStack)
	for (i in 1:rows) {
		light <- c(light, inputFunction(trialStack[i,]))
	}
	light
}

Beta <- function(lightInt, absCoeff, distance) {
	lightInt*exp(-absCoeff*distance^2)
}

fireFlyOpt <- function(count=100, maxGeneration=100, varCount=2, range = matrix(c(-5,-5,5,5),2,2), absCoeff=0.5 , randomStep=1, display=FALSE) {
	trialStack <- randomTrials(count, varCount, range)
	lightInt <- lightIntensity(trialStack)
	for (t in 1:maxGeneration) {
		
                if (display==TRUE) {
                        a <- seq(-5,5,0.1)
                        l <- length(a)
                        mat <- matrix(0,l, l)
                        for (i in 1:l)
                                for (j in 1:l)
                                        mat[i,j] = inputFunction(c(a[i],a[j]))
                        image(a,a,mat)
                        points(trialStack[,1],trialStack[,2],pch='+')
                }

			
		lightMax <- max(lightInt)
		lightMin <- min(lightInt)
		for (i in 1:count) {
			for (j in 1:count) {
				if (i!=j) {
					if (lightInt[j] < lightInt[i]) {
						distance <- euclDist (trialStack[i], trialStack[j])
						Li <- max ( min ((lightInt[i] - lightMin) / (lightMax - lightMin),1 ) ,0 )
						beta <- Beta( Li , absCoeff, distance)
						trialStack[j,] <- (1-beta)*trialStack[j,] + beta*trialStack[i,] + randomStep*( rep( runif(1)-0.5,2) )
						lightInt[j] <- inputFunction(trialStack[j,])
					}
				}
			}
		}
	}
        max_f <- max(lightInt)
        arg_max_f = unique ( trialStack[ which ( lightInt == max_f ),] )
        #output
        list( max = max_f , arg_max = arg_max_f )
}

Answer <- fireFlyOpt(count=100, maxGeneration=10, varCount=2, range = matrix(c(-5,-5,5,5),2,2), absCoeff=0.1 , randomStep=0.5, display=TRUE)
print(Answer)
