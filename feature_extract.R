#Read a Wave File
library(tuneR)
library(seewave)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(warbleR)
library(mice)
library(xgboost)
library(e1071)
extract_audio_features <- function(x) {
  #tuneR
  tr <- readWave(x) # load file
  #print(t@left)
  ar <- read.AsspDataObj(x)
  #File Name
  #fname <- file_path_sans_ext(basename(x))
  #add Feature Number of Samples
  
  fname <- 'male.wav'
  
  num_samples <- numRecs.AsspDataObj(ar)
  # calculate formants and corresponding bandwidth values
  fmBwVals <- forest(x,toFile=F)
  fmVals <- fmBwVals$fm
  bwVals <- fmBwVals$bw
  #add Feature Sample Rate
  sample_rate <- tr@samp.rate
  left= tr@left
  #left
  range_audio = range(tr@left)
  #add Feature min_amplitude_range
  min_range =range_audio[1]
  #add Feature min_amplitude_range
  max_range =range_audio[2]
  normvalues=left/2^(tr@bit -1)
  normal_range <- range(normvalues)
  #add Feature normalized_min_amplitude_range
  normal_min_ampl_range <- normal_range[1]
  #add Feature normalized_min_amplitude_range
  normal_max_ampl_range <- normal_range[2]
  mylist <- c(fname=fname,num_samples=num_samples,sample_rate=sample_rate, min_range=min_range, max_range=max_range, normal_min_ampl_range=normal_min_ampl_range, normal_max_ampl_range=normal_max_ampl_range,fmVals=fmVals,bwVals=bwVals)
  return(as.data.frame(mylist))
}
#path of file
file_audio_path <- 'F:/female3.wav'
#Read Files
output = extract_audio_features(file_audio_path)
head(output,10)
r <- tuneR::readWave('F:/female3.wav', 0, 1, units = "seconds")
songspec <- seewave::spec(r, f = r@samp.rate, plot = FALSE)
analysis <- seewave::specprop(songspec, f = r@samp.rate, flim = c(0, 280/1000), plot = FALSE)
meanfreq <- analysis$mean/1000
sd <- analysis$sd/1000
median <- analysis$median/1000
Q25 <- analysis$Q25/1000
Q75 <- analysis$Q75/1000
IQR <- analysis$IQR/1000
skew <- analysis$skewness
kurt <- analysis$kurtosis
sp.ent <- analysis$sh
sfm <- analysis$sfm
mode <- analysis$mode/1000
centroid <- analysis$cent/1000
peakf <- 0#seewave::fpeaks(songspec, f = r@samp.rate, wl = wl, nmax = 3, plot = FALSE)[1, 1]

#Fundamental frequency parameters
ff <- seewave::fund(r, f = r@samp.rate, ovlp = 50, 
                    fmax = 280, ylim=c(0, 280/1000), plot = FALSE )[, 2]
meanfun<-mean(ff, na.rm = T)
minfun<-min(ff, na.rm = T)
maxfun<-max(ff, na.rm = T)
y <- seewave::dfreq(r, f = r@samp.rate, ylim=c(0, 280/1000), ovlp = 0, plot = F, fftw = TRUE)[, 2]
meandom <- mean(y, na.rm = TRUE)
mindom <- min(y, na.rm = TRUE)
maxdom <- max(y, na.rm = TRUE)
dfrange <- (maxdom - mindom)
head(sd)
head(median)
head(Q25)
head(Q75)
head(IQR)
head(skew)
head(sp.ent)
head(mode)
head(centroid)
head(meanfun)
head(minfun)
head(maxfun)
head(mindom)
head(maxdom)