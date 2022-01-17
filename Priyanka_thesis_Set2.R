## Master's Thesis: Predicting Startup Investments From Facial 
## Expressions of Entrepreneurs Using Recurrent Neural Networks

## Priyanka Vinod (2065580)

## M.Sc. Data Science & Society, Tilburg University

## Code for data pre-processing of entrepreneurial pitch data

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

## Load OpenFace data ----------------------------------------------------------
of_pitch1  <- read.csv("data/OpenFace/1_Little_Sister.csv")
of_pitch2  <- read.csv("data/OpenFace/2_FLIPR.csv")
of_pitch3  <- read.csv("data/OpenFace/3_Bubble_Pop.csv")
of_pitch4  <- read.csv("data/OpenFace/4_RecognEyes.csv")
of_pitch5  <- read.csv("data/OpenFace/5_HOTIDY.csv")
of_pitch6  <- read.csv("data/OpenFace/6_FitPoint.csv")
of_pitch7  <- read.csv("data/OpenFace/7_SOLON.csv")
of_pitch8  <- read.csv("data/OpenFace/8_tAIste.csv")
of_pitch9  <- read.csv("data/OpenFace/9_Choos3_Wisely.csv")
of_pitch10 <- read.csv("data/OpenFace/10_SmArt.csv")
of_pitch11 <- read.csv("data/OpenFace/12_wAIste.csv")
of_pitch12 <- read.csv("data/OpenFace/13_Chattern.csv")
of_pitch13 <- read.csv("data/OpenFace/14_FindIT.csv")
of_pitch14 <- read.csv("data/OpenFace/15_Ar-T-ficial.csv")
of_pitch15 <- read.csv("data/OpenFace/16_Recipe-Me.csv")
of_pitch16 <- read.csv("data/OpenFace/17_Salix.csv")
of_pitch17 <- read.csv("data/OpenFace/18_Peech.csv")
of_pitch18 <- read.csv("data/OpenFace/19_HoodFood.csv")
of_pitch19 <- read.csv("data/OpenFace/20_LockUp.csv")
of_pitch20 <- read.csv("data/OpenFace/21_Ziggurat.csv")
of_pitch21 <- read.csv("data/OpenFace/22_PREA.csv")
of_pitch22 <- read.csv("data/OpenFace/23_Young_Boosters.csv")
of_pitch23 <- read.csv("data/OpenFace/24_Whitebox.csv")
of_pitch24 <- read.csv("data/OpenFace/25_Soccer_Academy.csv")

## Load FaceReader data --------------------------------------------------------
fr_pitch1  <- read.csv("data/FaceReader/1_Little_Sister.txt", skip = 8,
                       sep = "\t")
fr_pitch2  <- read.csv("data/FaceReader/2_FLIPR.txt", skip = 8, 
                       sep = "\t")
fr_pitch3  <- read.csv("data/FaceReader/3_Bubble_Pop.txt", skip = 8, 
                       sep = "\t")
fr_pitch4  <- read.csv("data/FaceReader/4_RecognEyes.txt", skip = 8, 
                       sep = "\t")
fr_pitch5  <- read.csv("data/FaceReader/5_HOTIDY.txt", skip = 8, 
                       sep = "\t")
fr_pitch6  <- read.csv("data/FaceReader/6_FitPoint.txt", skip = 8, 
                       sep = "\t")
fr_pitch7  <- read.csv("data/FaceReader/7_SOLON.txt", skip = 8, 
                       sep = "\t")
fr_pitch8  <- read.csv("data/FaceReader/8_tAIste.txt", skip = 8, 
                       sep = "\t")
fr_pitch9  <- read.csv("data/FaceReader/9_Choos3_Wisely.txt", skip = 8, 
                       sep = "\t")
fr_pitch10 <- read.csv("data/FaceReader/10_SmArt.txt", skip = 8, 
                       sep = "\t")
fr_pitch11 <- read.csv("data/FaceReader/12_wAIste.txt", skip = 8, 
                       sep = "\t")
fr_pitch12 <- read.csv("data/FaceReader/13_Chattern.txt", skip = 8, 
                       sep = "\t")
fr_pitch13 <- read.csv("data/FaceReader/14_FindIT.txt", skip = 8, 
                       sep = "\t")
fr_pitch14 <- read.csv("data/FaceReader/15_Ar-T-ficial.txt", skip = 8, 
                       sep = "\t")
fr_pitch15 <- read.csv("data/FaceReader/16_Recipe-Me.txt", skip = 8, 
                       sep = "\t")
fr_pitch16 <- read.csv("data/FaceReader/17_Salix.txt", skip = 8, 
                       sep = "\t")
fr_pitch17 <- read.csv("data/FaceReader/18_Peech.txt", skip = 8, 
                       sep = "\t")
fr_pitch18 <- read.csv("data/FaceReader/19_HoodFood.txt", skip = 8, 
                       sep = "\t")
fr_pitch19 <- read.csv("data/FaceReader/20_LockUp.txt", skip = 8, 
                       sep = "\t")
fr_pitch20 <- read.csv("data/FaceReader/21_Ziggurat.txt", skip = 8, 
                       sep = "\t")
fr_pitch21 <- read.csv("data/FaceReader/22_PREA.txt", skip = 8, 
                       sep = "\t")
fr_pitch22 <- read.csv("data/FaceReader/23_Young_Boosters.txt", skip = 8, 
                       sep = "\t")
fr_pitch23 <- read.csv("data/FaceReader/24_Whitebox.txt", skip = 8, 
                       sep = "\t")
fr_pitch24 <- read.csv("data/FaceReader/25_Soccer_Academy.txt", skip = 8, 
                       sep = "\t")

## Select required features for each experiment from all the datasets ----------
of_features <- c('frame', 'timestamp', 'AU01_r', 'AU02_r', 'AU04_r', 'AU05_r', 
                  'AU06_r', 'AU07_r', 'AU12_r', 'AU15_r', 'AU20_r', 'AU23_r', 
                  'AU26_r')
fr_cols <- c(1, 20:25, 28, 30, 33, 34, 37)
fr_features <- colnames(fr_pitch1)[fr_cols]
fr_features

## Select required rows of pitch1 ----------------------------------------------
of_pitch_1 <- of_pitch1 %>%
  select(of_features) %>%
  mutate(y = 1, index = 1) %>%
  slice(1201:5700)

fr_pitch_1 <- fr_pitch1 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 1) %>%
  slice(1441:5940)   

## Select required rows of pitch2 ----------------------------------------------
of_pitch_2 <- of_pitch2 %>%
  select(of_features) %>%
  mutate(y = 1, index = 2) %>%
  slice(101:4600)

fr_pitch_2 <- fr_pitch2 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 2) %>%
  slice(201:4700)    

## Select required rows of pitch3 ----------------------------------------------
of_pitch_3 <- of_pitch3 %>%
  select(of_features) %>%
  mutate(y = 0, index = 3) %>%
  slice(301:4800)

fr_pitch_3 <- fr_pitch3 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 0, index = 3) %>%
  slice(301:4800)    

## Select required rows of pitch4 ----------------------------------------------
of_pitch_4 <- of_pitch4 %>%
  select(of_features) %>%
  mutate(y = 1, index = 4) %>%
  slice(0:4500)

fr_pitch_4 <- fr_pitch4 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 4) %>%
  slice(0:4500)

## Select required rows of pitch5 ----------------------------------------------
of_pitch_5 <- of_pitch5 %>%
  select(of_features) %>%
  mutate(y = 1, index = 5) %>%
  slice(76:4575)

fr_pitch_5 <- fr_pitch5 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 5) %>%
  slice(91:4590)

## Select required rows of pitch6 ----------------------------------------------
of_pitch_6 <- of_pitch6 %>%
  select(of_features) %>%
  mutate(y = 1, index = 6) %>%
  slice(106:4605)

fr_pitch_6 <- fr_pitch6 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 6) %>%
  slice(127:4626)

## Select required rows of pitch7 ----------------------------------------------
of_pitch_7 <- of_pitch7 %>%
  select(of_features) %>%
  mutate(y = 1, index = 7) %>%
  slice(361:4860)

fr_pitch_7 <- fr_pitch7 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 7) %>%
  slice(433:4932) 

## Select required rows of pitch8 ----------------------------------------------
of_pitch_8 <- of_pitch8 %>%
  select(of_features) %>%
  mutate(y = 0, index = 8) %>%
  slice(1981:6480)

fr_pitch_8 <- fr_pitch8 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 0, index = 8) %>%
  slice(2762:7261) 

## Select required rows of pitch9 ----------------------------------------------
of_pitch_9 <- of_pitch9 %>%
  select(of_features) %>%
  mutate(y = 1, index = 9) %>%
  slice(0:4500)

fr_pitch_9 <- fr_pitch9 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 9) %>%
  slice(0:4500) 

## Select required rows of pitch10 ---------------------------------------------
of_pitch_10 <- of_pitch10 %>%
  select(of_features) %>%
  mutate(y = 0, index = 10) %>%
  slice(0:4500)

fr_pitch_10 <- fr_pitch10 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 0, index = 10) %>%
  slice(0:4500) 

## Select required rows of pitch11 ---------------------------------------------
of_pitch_11 <- of_pitch11 %>%
  select(of_features) %>%
  mutate(y = 1, index = 11) %>%
  slice(0:4500)

fr_pitch_11 <- fr_pitch11 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 11) %>%
  slice(0:4500) 

## Select required rows of pitch12 ---------------------------------------------
of_pitch_12 <- of_pitch12 %>%
  select(of_features) %>%
  mutate(y = 1, index = 12) %>%
  slice(0:4500)

fr_pitch_12 <- fr_pitch11 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 12) %>%
  slice(0:4500) 

## Select required rows of pitch13 ---------------------------------------------
of_pitch_13 <- of_pitch13 %>%
  select(of_features) %>%
  mutate(y = 0, index = 13) %>%
  slice(0:4500)

fr_pitch_13 <- fr_pitch13 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 0, index = 13) %>%
  slice(0:4500) 

## Select required rows of pitch14 ---------------------------------------------
of_pitch_14 <- of_pitch14 %>%
  select(of_features) %>%
  mutate(y = 1, index = 14) %>%
  slice(0:4500)

fr_pitch_14 <- fr_pitch14 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 14) %>%
  slice(0:4500) 

## Select required rows of pitch15 ---------------------------------------------
of_pitch_15 <- of_pitch15 %>%
  select(of_features) %>%
  mutate(y = 0, index = 15) %>%
  slice(0:4500)

fr_pitch_15 <- fr_pitch15 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 0, index = 15) %>%
  slice(0:4500) 

## Select required rows of pitch16 ---------------------------------------------
of_pitch_16 <- of_pitch16 %>%
  select(of_features) %>%
  mutate(y = 1, index = 16) %>%
  slice(0:4500)

fr_pitch_16 <- fr_pitch16 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 16) %>%
  slice(0:4500) 

## Select required rows of pitch17 ---------------------------------------------
of_pitch_17 <- of_pitch17 %>%
  select(of_features) %>%
  mutate(y = 1, index = 17) %>%
  slice(0:4500)

fr_pitch_17 <- fr_pitch17 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 17) %>%
  slice(0:4500) 

## Select required rows of pitch18 ---------------------------------------------
of_pitch_18 <- of_pitch18 %>%
  select(of_features) %>%
  mutate(y = 0, index = 18) %>%
  slice(0:4500)

fr_pitch_18 <- fr_pitch18 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 0, index = 18) %>%
  slice(0:4500) 

## Select required rows of pitch19 ---------------------------------------------
of_pitch_19 <- of_pitch19 %>%
  select(of_features) %>%
  mutate(y = 0, index = 19) %>%
  slice(0:4500)

fr_pitch_19 <- fr_pitch19 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 0, index = 19) %>%
  slice(0:4500)

## Select required rows of pitch20 ---------------------------------------------
of_pitch_20 <- of_pitch20 %>%
  select(of_features) %>%
  mutate(y = 1, index = 20) %>%
  slice(46:4545)

fr_pitch_20 <- fr_pitch20 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 20) %>%
  slice(46:4545)

## Select required rows of pitch21 ---------------------------------------------
of_pitch_21 <- of_pitch21 %>%
  select(of_features) %>%
  mutate(y = 1, index = 21) %>%
  slice(0:4500)

fr_pitch_21 <- fr_pitch21 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 21) %>%
  slice(0:4500)

## Select required rows of pitch22 ---------------------------------------------
of_pitch_22 <- of_pitch22 %>%
  select(of_features) %>%
  mutate(y = 0, index = 22) %>%
  slice(0:4500)    

fr_pitch_22 <- fr_pitch22 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 0, index = 22) %>%
  slice(0:4500)

## Select required rows of pitch23 ---------------------------------------------
of_pitch_23 <- of_pitch23 %>%
  select(of_features) %>%
  mutate(y = 1, index = 23) %>%
  slice(0:4500)

fr_pitch_23 <- fr_pitch23 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 23) %>%
  slice(0:4500)

## Select required rows of pitch24 ---------------------------------------------
of_pitch_24 <- of_pitch24 %>%
  select(of_features) %>%
  mutate(y = 1, index = 24) %>%
  slice(0:4500)

fr_pitch_24 <- fr_pitch24 %>%
  select(all_of(fr_features)) %>%
  mutate(y = 1, index = 24) %>%
  slice(0:4500)

## bind rows
expt2_of <- bind_rows(of_pitch_1, of_pitch_2, of_pitch_3, of_pitch_4, 
                     of_pitch_5, of_pitch_6, of_pitch_7, of_pitch_8, 
                     of_pitch_9, of_pitch_10,  of_pitch_11, of_pitch_12, 
                     of_pitch_13, of_pitch_14, of_pitch_15, of_pitch_16, 
                     of_pitch_17, of_pitch_18, of_pitch_19, of_pitch_20, 
                     of_pitch_21, of_pitch_22, of_pitch_23, of_pitch_24)    

expt2_fr <- bind_rows(fr_pitch_1, fr_pitch_2, fr_pitch_3, fr_pitch_4, 
                     fr_pitch_5, fr_pitch_6, fr_pitch_7, fr_pitch_8, 
                     fr_pitch_9, fr_pitch_10, fr_pitch_11, fr_pitch_12, 
                     fr_pitch_13, fr_pitch_14, fr_pitch_15, fr_pitch_16, 
                     fr_pitch_17, fr_pitch_18, fr_pitch_19, fr_pitch_20, 
                     fr_pitch_21, fr_pitch_22, fr_pitch_23, fr_pitch_24) 
                    
## remove unwanted columns
expt2_of <- expt2_of %>%
  select(-c('frame', 'timestamp'))

expt2_fr <- expt2_fr %>%
  select(-c('Video.Time')) %>%
  mutate(across(everything(), as.numeric))

expt2_fr <- expt2_fr %>%
  mutate(across(everything(), ~replace_na(.x, 0))) 

## summary statistics
summary(expt2_of)
summary(expt2_fr)

## save files
write.csv(expt2_of, file = 'expt2_of.csv')
write.csv(expt2_fr, file = 'expt2_fr.csv')
# ---------------------------------------------------------------------------- #