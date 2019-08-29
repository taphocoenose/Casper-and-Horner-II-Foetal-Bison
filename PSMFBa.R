################################################################################
#  Probabilistic Seasonality Models Based on Fetal Bison antiquus Osteometrics #
################################################################################
#
# Built in R Version 3.5.1 - "Feather Spray"
# 
# Last updated on August 28, 2019.
# Tested on an Asus Zenbook Flip with Windows 10.
# 
# Report issues or make suggestions: rbreslawski@smu.edu
#
# Required libraries: ggplot2, gridExtra, reshape2, 
#                     quantreg, boot, smoother
#
#   This script produces season of death estimates (SODEs) based on fetal bison
# antiquus osteometrics. The osteometrics include minimum diaphyseal depths of
# tibiae, femora, radii, and humeri. Metric values are converted into gestation
# age ranges, as measured in days. These gestation age ranges are combined
# with conception calendars to estimate SODEs. The SODEs are  presented as
# probability masses apportioned over individual days in a calendar year.
#
#   This script is written as an interactive script that users can employ
# through the command line interface in the R GUI or RStudio. When the
# script is called, the user proceeds through the analysis guided by a
# series of prompts. Users are first presented with an introduction cons-
# isting of explanatory text. They are then prompted to select a calendar
# defining the distribution of gestation onset. Each calendar option is
# supported by ecological data on bison reproduction. After the calendar
# is selected, the user is prompted to enter elements and their length
# metrics. Entries may be made with text entered into the command line
# or via upload of an appropriately formatted csv table. The script will
# detect invalid entries and prompt the user to re-enter their data.
#
#   The script stores the entries and prints the gestation age ranges and
# death date ranges for each one. This is accompanied by graphical output
# displaying the probability mass distribution for each entry. The user
# is then prompted to complete further analyses or quit the script. 
#
#   Further analyses include the generation of probabilities based on user
# defined date intervals and combining elements from a single fetus to
# generate more constrained gestation ages, and thus more informative
# SODEs (if the user only entered one element, the script will only pres-
# ent the option for the date interval analysis). The date interval anal-
# ysis will present users with the probability that all elements represent
# deaths within the defined interval as well as the probability that all 
# elements represent a death on the same day within the interval. Probab-
# ilities estimating death on the same day for multiple distributions are 
# generally low relative to probabilities for intervals spanning multiple days.
#
#   The combined element procedure assumes that the user has entered multiple
# elements that were synchronously deposited, such as those belonging
# to a single fetus. Since these entries correspond to death date
# probability masses that estimate the same event, each distribution 
# can be used as a prior to obtain a more informative posterior estimation 
# of the date. The posterior distribution is saved as a new entry so that 
# the user can incorporate it into further analyses.
#
#   The majority of this script is contained within eight functions:
#
#   select.model: This function prompts users to select a calendar
#                 defining when gestation begins. It constructs
#                 conception calendars based on observations of
#                 modern bison herds.
#   
#   analysis.combined: This function performs the combined element analysis.
#   
#   analysis.interval: This function performs the date interval analysis.
#
#   entered.data: This function guides the user through manual entry or
#                 uploading a csv table containing osteometric data.
#
#   main.function: This function combines the variables returned from
#                  select.model and entered.data to print and graph
#                  the death date probability masses.
#
#   est.function: This function simulated tibia, femur, radius, and 
#                 humerus length values over a 320 day bison gestation
#                 period. It also gives users the option to plot these
#                 simulated values.
#
#   metric.function: This function estimates diaphysis lengths from
#                    minimum antero-posterior diaphysis depths.
#
#   bootratios: This function bootstraps long-bone length ratios for
#               antiquus/modern bison.
#
#   The introductory text for this program is located below all of the functions.
# The script is executed by first calling entered.data, which returns a vari-
# able that is automatically passed into main.function as an argument. When
# main.function is executed, it also executes select.model. The option to call
# analysis.combined, and analysis.interval is coded into the end of main.function
# as a series of user prompts.
#
#############################################################################

# Load libraries.
library(ggplot2)
library(gridExtra)
library(reshape2)
library(smoother)
library(boot)
library(quantreg)

# FUNCTION: This prompts the user to select a conception calendar.
# It does not require arguments. The function returns a vector of
# calendar date probabilities selected by the user. Data on bison
# conception schedules are coded into the function.
select.model <- function(){
  
  # Construct conception calendar vectors for different bison herds.
  # Each vector is 245 positions long, corresponding to an interval
  # spanning June 1 through January 31. The following blocks of code
  # describe conception calendars for bison in the Niobrara Valley
  # based on observed bull fights during the rut, at the National Bison 
  # Range based on observed bull-cow copulations, At Wind Cave National
  # Park/Custer State Park/Niobrara Valley based on fetal biometrics, 
  # and at northern and western Yellowstone Nation Park based on fetal 
  # biometrics.
  
  # Occurrences of bull fights during the rut in the Niobrara Valley.
  # Data from Wolff (1998:532).
  #
  # Data reference: Wolff JO, 1998. Breeding strategies, mate choice,
  #                 and reproductive success in American bison. Oikos
  #                 83, 529-544.
  Niob.bull.fights <- rep(0, 245)
  Niob.bull.fights[53] <- 3
  Niob.bull.fights[54] <- 1
  Niob.bull.fights[56] <- 1
  Niob.bull.fights[57] <- 7
  Niob.bull.fights[58] <- 2
  Niob.bull.fights[60] <- 7
  Niob.bull.fights[61] <- 7
  Niob.bull.fights[62] <- 3
  Niob.bull.fights[63] <- 13
  Niob.bull.fights[64] <- 9
  Niob.bull.fights[65] <- 43
  Niob.bull.fights[66] <- 8
  Niob.bull.fights[67] <- 25
  Niob.bull.fights[68] <- 17
  Niob.bull.fights[69] <- 14
  Niob.bull.fights[70] <- 17
  Niob.bull.fights[71] <- 33
  Niob.bull.fights[72] <- 24
  Niob.bull.fights[73] <- 24
  Niob.bull.fights[74] <- 33
  Niob.bull.fights[75] <- 25
  Niob.bull.fights[76] <- 18
  Niob.bull.fights[77] <- 28
  Niob.bull.fights[78] <- 23
  Niob.bull.fights[79] <- 6
  Niob.bull.fights[80] <- 9
  Niob.bull.fights[81] <- 17
  Niob.bull.fights[82] <- 11
  Niob.bull.fights[83] <- 4
  Niob.bull.fights[84] <- 5
  Niob.bull.fights[85] <- 10
  Niob.bull.fights[86] <- 9
  Niob.bull.fights[87] <- 3
  Niob.bull.fights[88] <- 4
  Niob.bull.fights[92] <- 1
  Niob.bull.fights[95] <- 2
  Niob.bull.fights[96] <- 1
  Niob.bull.fights[98] <- 2
  
  # Copulations per day for bison cows at the National Bison Range (MT).
  # Data originates from Lott (1981:105).
  #
  # Data reference: Lott DF, 1981. Sexual behavior and intersexual
  #                 strategies in American bison. Zeitshrift fur 
  #                 Tierpsychologie 56, 97-114.
  NBR.cops <- rep(0, 245)
  NBR.cops[56] <- 1
  NBR.cops[58] <- 1
  NBR.cops[59] <- 2
  NBR.cops[60] <- 4
  NBR.cops[61] <- 4
  NBR.cops[62] <- 4
  NBR.cops[63] <- 5
  NBR.cops[64] <- 2
  NBR.cops[65] <- 3
  NBR.cops[66] <- 1
  NBR.cops[68] <- 2
  NBR.cops[70] <- 1
  NBR.cops[71] <- 5
  NBR.cops[72] <- 1
  NBR.cops[73] <- 1
  
  # Conception date estimations for 131 fetuses from Wind Cave National Park (SD), 
  # Custer State Park (SD), and Fort Niobrara (NE) (based on a 285 day gestation 
  # length). Data are reported in Haugen (1974), who lists conceptions in 5-day 
  # intervals. Here, the number of conceptions is distributed evenly across each 
  # day in each 5-day interval. The data were not separated by herd. However, in 
  # reference to sex ratios, Haugen (1974:3) reports that 63 originated from Wind 
  # Cave, 4 from Niobrara, and 34 from Custer State Park (an additional 30 fetuses 
  # are not accounted for or were not assigned a sex). In short, these data are mostly 
  # representative of the Wind Cave herd, and to a lesser extent the Custer 
  # State Park herd. 
  #
  # Data reference: Haugen, AO, 1974. Reproduction in the Plains bison. Iowa
  #                 State J. Res. 49, 1-8.
  Assrtd.conception.est <- rep(0, 245)
  Assrtd.conception.est[31:35] <- 1/5
  Assrtd.conception.est[36:40] <- 1/5
  Assrtd.conception.est[41:45] <- 1/5
  Assrtd.conception.est[46:50] <- 2/5
  Assrtd.conception.est[51:55] <- 8/5
  Assrtd.conception.est[56:60] <- 20/5
  Assrtd.conception.est[61:65] <- 32/5
  Assrtd.conception.est[66:70] <- 20/5
  Assrtd.conception.est[71:75] <- 15/5
  Assrtd.conception.est[76:80] <- 5/5
  Assrtd.conception.est[81:85] <- 1/5
  Assrtd.conception.est[86:90] <- 10/5
  Assrtd.conception.est[91:95] <- 7/5
  Assrtd.conception.est[96:100] <- 3/5
  Assrtd.conception.est[101:105] <- 2/5
  Assrtd.conception.est[106:110] <- 1/5
  Assrtd.conception.est[111:115] <- 1/5
  Assrtd.conception.est[121:125] <- 1/5
  
  # The following fetal conception dates are based on YNP birth dates 
  # estimated by Gogan et al. (2005). The estimated dates of conception
  # events are backcalculated by taking the parturition events estimated
  # by Gogan et al., and subtracting the historic and modern gestation
  # lengths from these dates. Since they present births in weekly bins,
  # here, the number of events is distributed evenly across each week day.
  #
  # Although each conception calendar is initialized with 0s, 0s are
  # reassigned whenever a given herd/year did not have births for a
  # given week. This keeps the herd/year data format consistent for 
  # the data assignment text blocks under each week.
  #
  # Data reference: Gogan PJP, Podruzny KM, Olexa EM, Pac HI, Frey
  #                 KL, 2005. Yellowstone bison fetal development 
  #                 and phenology of parturition. J. Wildl. Manag. 
  #                 69, 1716-1730.
  YNP.W.2002 <- YNP.W.1999 <- YNP.W.1997 <- YNP.N.1997 <- YNP.W.1996 <- 
    YNP.W.1995 <- YNP.N.1989 <- YNP.N.1941 <- rep(0, 245)
  
  # YNP herds week 1.
  YNP.N.1941[34:40] <- 1/7
  YNP.N.1989[27:33] <- 1/7
  YNP.W.1995[27:33] <- 0
  YNP.W.1996[27:33] <- 0
  YNP.N.1997[27:33] <- 0
  YNP.W.1997[27:33] <- 0
  YNP.W.1999[27:33] <- 0
  YNP.W.2002[27:33] <- 0
  # YNP herds week 2.
  YNP.N.1941[41:47] <- 13/7
  YNP.N.1989[34:40] <- 3/7
  YNP.W.1995[34:40] <- 0
  YNP.W.1996[34:40] <- 0
  YNP.N.1997[34:40] <- 0
  YNP.W.1997[34:40] <- 0
  YNP.W.1999[34:40] <- 0
  YNP.W.2002[34:40] <- 0
  # YNP herds week 3.
  YNP.N.1941[48:54] <- 11/7
  YNP.N.1989[41:47] <- 3/7
  YNP.W.1995[41:47] <- 0
  YNP.W.1996[41:47] <- 0
  YNP.N.1997[41:47] <- 1/7
  YNP.W.1997[41:47] <- 0
  YNP.W.1999[41:47] <- 1/7
  YNP.W.2002[41:47] <- 1/7
  # YNP herds week 4.
  YNP.N.1941[55:61] <- 7/7
  YNP.N.1989[48:54] <- 9/7
  YNP.W.1995[48:54] <- 0
  YNP.W.1996[48:54] <- 0
  YNP.N.1997[48:54] <- 3/7
  YNP.W.1997[48:54] <- 0
  YNP.W.1999[48:54] <- 4/7
  YNP.W.2002[48:54] <- 3/7
  # YNP herds week 5.
  YNP.N.1941[62:68] <- 9/7
  YNP.N.1989[55:61] <- 11/7
  YNP.W.1995[55:61] <- 1/7
  YNP.W.1996[55:61] <- 0
  YNP.N.1997[55:61] <- 7/7
  YNP.W.1997[55:61] <- 1/7
  YNP.W.1999[55:61] <- 1/7
  YNP.W.2002[55:61] <- 3/7
  # YNP herds week 6.
  YNP.N.1941[69:75] <- 8/7
  YNP.N.1989[62:68] <- 2/7
  YNP.W.1995[62:68] <- 1/7
  YNP.W.1996[62:68] <- 2/7
  YNP.N.1997[62:68] <- 21/7
  YNP.W.1997[62:68] <- 2/7
  YNP.W.1999[62:68] <- 10/7
  YNP.W.2002[62:68] <- 4/7
  # YNP herds week 7.
  YNP.N.1941[76:82] <- 10/7
  YNP.N.1989[69:75] <- 4/7
  YNP.W.1995[69:75] <- 0
  YNP.W.1996[69:75] <- 2/7
  YNP.N.1997[69:75] <- 15/7
  YNP.W.1997[69:75] <- 4/7
  YNP.W.1999[69:75] <- 7/7
  YNP.W.2002[69:75] <- 2/7
  # YNP herds week 8.
  YNP.N.1941[83:89] <- 5/7
  YNP.N.1989[76:82] <- 4/7
  YNP.W.1995[76:82] <- 0
  YNP.W.1996[76:82] <- 2/7
  YNP.N.1997[76:82] <- 9/7
  YNP.W.1997[76:82] <- 5/7
  YNP.W.1999[76:82] <- 2/7
  YNP.W.2002[76:82] <- 6/7
  # YNP herds week 9.
  YNP.N.1941[90:96] <- 4/7
  YNP.N.1989[83:89] <- 4/7
  YNP.W.1995[83:89] <- 0
  YNP.W.1996[83:89] <- 1/7
  YNP.N.1997[83:89] <- 6/7
  YNP.W.1997[83:89] <- 4/7
  YNP.W.1999[83:89] <- 0
  YNP.W.2002[83:89] <- 1/7
  # YNP herds week 10.
  YNP.N.1941[97:103] <- 2/7
  YNP.N.1989[90:96] <- 3/7
  YNP.W.1995[90:96] <- 0
  YNP.W.1996[90:96] <- 0
  YNP.N.1997[90:96] <- 3/7
  YNP.W.1997[90:96] <- 10/7
  YNP.W.1999[90:96] <- 3/7
  YNP.W.2002[90:96] <- 1/7
  # YNP herds week 11.
  YNP.N.1941[104:110] <- 1/7
  YNP.N.1989[97:103] <- 0
  YNP.W.1995[97:103] <- 1/7
  YNP.W.1996[97:103] <- 2/7
  YNP.N.1997[97:103] <- 2/7
  YNP.W.1997[97:103] <- 2/7
  YNP.W.1999[97:103] <- 0
  YNP.W.2002[97:103] <- 0
  # YNP herds week 12.
  YNP.N.1941[111:117] <- 0
  YNP.N.1989[104:110] <- 1/7
  YNP.W.1995[104:110] <- 0
  YNP.W.1996[104:110] <- 0
  YNP.N.1997[104:110] <- 0
  YNP.W.1997[104:110] <- 0
  YNP.W.1999[104:110] <- 0
  YNP.W.2002[104:110] <- 1/7
  # YNP herds week 13.
  YNP.N.1941[118:124] <- 0
  YNP.N.1989[111:117] <- 1/7
  YNP.W.1995[111:117] <- 0
  YNP.W.1996[111:117] <- 0
  YNP.N.1997[111:117] <- 0
  YNP.W.1997[111:117] <- 0
  YNP.W.1999[111:117] <- 1/7
  YNP.W.2002[111:117] <- 0
  # YNP herds week 14.
  YNP.N.1941[125:131] <- 2/7
  YNP.N.1989[118:124] <- 1/7
  YNP.W.1995[118:124] <- 0
  YNP.W.1996[118:124] <- 1/7
  YNP.N.1997[118:124] <- 1/7
  YNP.W.1997[118:124] <- 0
  YNP.W.1999[118:124] <- 0
  YNP.W.2002[118:124] <- 1/7
  # YNP herds week 15.
  YNP.N.1941[132:138] <- 1/7
  YNP.N.1989[125:131] <- 0
  YNP.W.1995[125:131] <- 0
  YNP.W.1996[125:131] <- 0
  YNP.N.1997[125:131] <- 0
  YNP.W.1997[125:131] <- 0
  YNP.W.1999[125:131] <- 1/7
  YNP.W.2002[125:131] <- 0
  # YNP herds week 16.
  YNP.N.1941[139:145] <- 0
  YNP.N.1989[132:138] <- 1/7
  YNP.W.1995[132:138] <- 0
  YNP.W.1996[132:138] <- 0
  YNP.N.1997[132:138] <- 1/7
  YNP.W.1997[132:138] <- 0
  YNP.W.1999[132:138] <- 1/7
  YNP.W.2002[132:138] <- 1/7
  # YNP herds week 17.
  YNP.N.1941[146:152] <- 0
  YNP.N.1989[139:145] <- 0
  YNP.W.1995[139:145] <- 0
  YNP.W.1996[139:145] <- 0
  YNP.N.1997[139:145] <- 0
  YNP.W.1997[139:145] <- 0
  YNP.W.1999[139:145] <- 0
  YNP.W.2002[139:145] <- 2/7
  # YNP herds week 18.
  YNP.N.1941[153:159] <- 0
  YNP.N.1989[146:152] <- 1/7
  YNP.W.1995[146:152] <- 0
  YNP.W.1996[146:152] <- 0
  YNP.N.1997[146:152] <- 0
  YNP.W.1997[146:152] <- 0
  YNP.W.1999[146:152] <- 0
  YNP.W.2002[146:152] <- 0
  # YNP herds week 19.
  YNP.N.1941[160:166] <- 0
  YNP.N.1989[153:159] <- 0
  YNP.W.1995[153:159] <- 0
  YNP.W.1996[153:159] <- 0
  YNP.N.1997[153:159] <- 0
  YNP.W.1997[153:159] <- 0
  YNP.W.1999[153:159] <- 0
  YNP.W.2002[153:159] <- 1/7
  # YNP herds week 20.
  YNP.N.1941[167:173] <- 0
  YNP.N.1989[160:166] <- 0
  YNP.W.1995[160:166] <- 0
  YNP.W.1996[160:166] <- 0
  YNP.N.1997[160:166] <- 0
  YNP.W.1997[160:166] <- 0
  YNP.W.1999[160:166] <- 0
  YNP.W.2002[160:166] <- 1/7
  # YNP herds week 21.
  YNP.N.1941[181:187] <- 0
  YNP.N.1989[174:180] <- 0
  YNP.W.1995[174:180] <- 0
  YNP.W.1996[174:180] <- 0
  YNP.N.1997[174:180] <- 0
  YNP.W.1997[174:180] <- 0
  YNP.W.1999[174:180] <- 0
  YNP.W.2002[174:180] <- 1/7
  # YNP herds week 22.
  YNP.N.1941[209:215] <- 0
  YNP.N.1989[202:208] <- 0
  YNP.W.1995[202:208] <- 0
  YNP.W.1996[202:208] <- 0
  YNP.N.1997[202:208] <- 0
  YNP.W.1997[202:208] <- 0
  YNP.W.1999[202:208] <- 0
  YNP.W.2002[202:208] <- 1/7
  
  # Aggregate YNP herd data into northern and western herds.
  YNP.sum.N <- YNP.N.1941+YNP.N.1989+YNP.N.1997
  YNP.sum.W <- YNP.W.1995+YNP.W.1996+YNP.W.1997+YNP.W.1999+YNP.W.2002
  
  # Standardize each calendar so that the values sum to 1. This allows
  # each calendar to be treated as a probability mass.
  Niob.prob <- Niob.bull.fights/sum(Niob.bull.fights)
  Assorted.prob <- Assrtd.conception.est/sum(Assrtd.conception.est)
  NBR.prob <- NBR.cops/sum(NBR.cops)
  YNP.N.prob <- YNP.sum.N/sum(YNP.sum.N)
  YNP.W.prob <- YNP.sum.W/sum(YNP.sum.W)
  
  # Create generalized YNP dataset from the standardized 
  # northern and western YNP herd data.
  YNP.sum.prob <- (YNP.sum.N+YNP.sum.W)/sum(YNP.sum.N+YNP.sum.W)
  
  # Aggregate the fetal data calendars into one generalized calendar.
  # Since the assorted fetal data originates from two herds, it recieves
  # twice the weighting of each YNP herd.
  Agg.ft.prob <- ((Assorted.prob*2)+YNP.N.prob+YNP.W.prob)/4
  
  # Create smoothed calendars with a 3-week kernel.
  Niob.smooth <- smth(Niob.prob, window=21, method="gaussian")
  NBR.smooth <- smth(NBR.prob, window=21, method="gaussian")
  Assort.smooth <- smth(Assorted.prob, window=21, method="gaussian")
  YNP.smooth <- smth(YNP.sum.prob, window=21, method="gaussian")
  YNP.N.smooth <- smth(YNP.N.prob, window=21, method="gaussian")
  YNP.W.smooth <- smth(YNP.W.prob, window=21, method="gaussian")
  AggF.smooth <- smth(Agg.ft.prob, window=21, method="gaussian")
  
  # The smoothing function generates NAs at the end of each vector
  # of smoothed values. Replace these NAs with 0s.
  NBR.smooth[is.na(NBR.smooth)] <- Niob.smooth[is.na(Niob.smooth)] <- 
    Assort.smooth[is.na(Assort.smooth)] <- YNP.smooth[is.na(YNP.smooth)] <-
    YNP.N.smooth[is.na(YNP.N.smooth)] <- YNP.W.smooth[is.na(YNP.W.smooth)] <-
    AggF.smooth[is.na(AggF.smooth)] <- 0
  
  # The smoothed distributions should sum to 1. However, the smoothing
  # function introduces minor rounding errors. These lines of code
  # restandardize each distribution. Most probabilities within each
  # distribution remain unchanged from their previous values, but some
  # are adjusted on the order of 10E-16 to 10E-20 in the process.
  Niob.smooth <- Niob.smooth/sum(Niob.smooth)
  NBR.smooth <- NBR.smooth/sum(NBR.smooth)
  Assort.smooth <- Assort.smooth/sum(Assort.smooth)
  YNP.smooth <- YNP.smooth/sum(YNP.smooth)
  YNP.N.smooth <- YNP.N.smooth/sum(YNP.N.smooth)
  YNP.W.smooth <- YNP.W.smooth/sum(YNP.W.smooth)
  AggF.smooth <- AggF.smooth/sum(AggF.smooth)
  
  # Assign calendars to a data frame.
  plotdata.fet <- data.frame(Agg.ft.prob, AggF.smooth, 
                     YNP.sum.prob, YNP.smooth, YNP.N.prob, YNP.N.smooth, 
                     YNP.W.prob, YNP.W.smooth, Assorted.prob, Assort.smooth, 
                     Niob.prob, Niob.smooth, NBR.prob, NBR.smooth)
	
  # Append a column of 245 days to plotdata.fet, where each day x is centered on
  # x-0.5. This is done so that the step geometry presented in the following
  # plots displays horizontal segments centered over each daily value.
  plotdata.fet$Days.plot <- seq(from=0.5, to=244.5, by=1)
  
	# A data frame with these altered daily values is subsetted between July 1 and
	# Sept 30. This is for the conception data that are constrained within a narrow 3
	# 3 month date interval (bull fights and copulations).
	plotdata.prox <- plotdata.fet[31:122,]

	# Present user with text describing each conception schedule.
	cat("\n[1]  Aggregated YNP, Custer State Park, and Wind Cave herds,",
      	"based on fetal data (n = 428) [sample data].\n")
	cat("[2]  Aggregated YNP, Custer State Park, and Wind Cave herds,",
      	"based on fetal data (n = 428) [3 week smooth].\n")
	cat("[3]  YNP western and northern herds, based on fetal metrics",
      	"(n = 297) [sample data].\n")
	cat("[4]  YNP western and northern herds, based on fetal metrics",
      	"(n = 297) [3 week smooth].\n")
	cat("[5]  YNP northern herd, based on fetal metrics (n = 192)",
	     "[sample data].\n")
	cat("[6]  YNP northern herd, based on fetal metrics (n = 192)",
      	"[3 week smooth].\n")
	cat("[7]  YNP western herd, based on fetal metrics (n = 105)",
      	"[sample data].\n")
	cat("[8]  YNP western herd, based on fetal metrics (n = 105)",
      	"[3 week smooth].\n")
	cat("[9]  Custer State Park and Wind Cave, based on fetal metrics",
      	"(n = 131) [sample data].\n")
	cat("[10] Custer State Park and Wind Cave, based on fetal metrics",
      	"(n = 131) [3 week smooth].\n")
	cat("[11] Niobrara bull fights (n = 1088)[sample data].\n")
	cat("[12] Niobrara bull fights (n = 1088)[3 week smooth].\n")
	cat("[13] National Bison Range copulations (n = 37)[sample data].\n")
	cat("[14] National Bison Range copulations (n = 37)[3 week smooth].\n\n")
	cat("Select a conception distribution (refer to plots for visual reference, expand or zoom",
	    "\nfor a better view of each calendar model. Y-axis days are zeroed on June 1). The aggreg-",
	    "\nated fetal samples gives the most 'generalized' distribution (Index 1). The smoothed",
      "\nversion of this distribution approximates the population from which the sample data were",
      "\ndrawn (Index 2).",
	    "\n\nGenerating conception calendar plots...\n\n")

	# Create vectors that define plot objects for months.
	pl.m.names <- c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan") # month names
	pl.m.xpos <- c(16, 46, 76, 107, 137, 168, 198, 228) # x position of month names
	pl.m.x1 <- c(1, 31, 62, 93, 123, 154, 184, 215) # start x position of background geometry for months
	pl.m.x2 <- c(29, 60, 91, 121, 152, 182, 213, 244) # end x position of background geometry for months
	
	# Create plots for each conception schedule subset. Each plot contains annotated rectangles
	# and text indicating months within the conception period. Both the raw and smoothed data
	# data are presented for each plot.	
	
	# Plot of aggregated YNP fetal data.
	YNP.sum.plot <- ggplot()+
	  annotate("text", x=pl.m.xpos, y=max(plotdata.fet$YNP.sum.prob)*1.09, 
	           label=pl.m.names, size=2.7)+
	  annotate("rect", xmin = pl.m.x1, xmax = pl.m.x2, 
	           ymin=max(plotdata.fet$YNP.sum.prob)*1.02, 
	           ymax=max(plotdata.fet$YNP.sum.prob)*1.15, alpha = 0.250)+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=YNP.sum.prob), 
	            stat="identity", color="red", alpha=0.7, direction="hv")+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=YNP.smooth), 
	            stat="identity", direction="hv")+
	  annotate("text", x=105, y=0.023, label="YNP herds", 
	           hjust=0, size=2.6)+
	  annotate("text", x=105, y=0.022, label="(3 week smooth)[5]", 
	           hjust=0, size=2.6)+
	  annotate("text", x=105, y=0.018, label="YNP herds", 
	           hjust=0, color="red", alpha=0.8, size=2.6)+
	  annotate("text", x=105, y=0.017, label="(sample data)[4]", 
	           hjust=0, color="red", alpha=0.8, size=2.6)+
	  xlab("")+
	  theme(axis.line.x = element_line(colour = "grey"),
	        axis.line.y = element_line(colour = "grey"),
	        panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(),
	        panel.border = element_rect(colour = "grey", fill=NA),
	        panel.background = element_blank(),
	        axis.title.y=element_blank())+
	  scale_x_continuous(breaks=c(30,92,153,214), expand=c(0.01,0),
	                     labels=c(181,243,304,365), limits=c(0,245))+
	  scale_y_continuous(expand=c(0.02,0))
	
	# Plot of fetal data for the northern YNP herd.
	YNP.N.plot <- ggplot()+
	  annotate("text", x=pl.m.xpos, y=max(plotdata.fet$YNP.N.prob)*1.09, 
	           label=pl.m.names, size=2.7)+
	  annotate("rect", xmin = pl.m.x1, xmax = pl.m.x2, 
	           ymin=max(plotdata.fet$YNP.N.prob)*1.02, 
	           ymax=max(plotdata.fet$YNP.N.prob)*1.15, alpha = 0.250)+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=YNP.N.prob), 
	            stat="identity", color="red", alpha=0.7, direction="hv")+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=YNP.N.smooth),
	            stat="identity", direction="hv")+
	  annotate("text", x=100, y=0.023, label="YNP North herd", 
	           hjust=0, size=2.6)+
	  annotate("text", x=100, y=0.022, label="(3 week smooth)[7]", 
	           hjust=0, size=2.6)+
	  annotate("text", x=100, y=0.018, label="YNP North herd",
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  annotate("text", x=100, y=0.017, label="(sample data)[6]",
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  xlab("")+
	  theme(axis.line.x = element_line(colour = "grey"),
	        axis.line.y = element_line(colour = "grey"),
	        panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(),
	        panel.border = element_rect(colour = "grey", fill=NA),
	        panel.background = element_blank(),
	        axis.title.y=element_blank())+
	  scale_x_continuous(breaks=c(30,92,153,214), expand=c(0.01,0),
	                     labels=c(181,243,304,365), limits=c(0,245))+
	  scale_y_continuous(expand=c(0.02,0))
	
	# Plot of fetal data for the western YNP herd.
	YNP.W.plot <- ggplot()+
	  annotate("text", x=pl.m.xpos, y=max(plotdata.fet$YNP.W.prob)*1.09, 
	           label=pl.m.names, size=2.7)+
	  annotate("rect", xmin = pl.m.x1, xmax = pl.m.x2, 
	           ymin=max(plotdata.fet$YNP.W.prob)*1.02, 
	           ymax=max(plotdata.fet$YNP.W.prob)*1.15, alpha = 0.250)+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=YNP.W.prob), 
	            stat="identity", color="red", alpha=0.7, direction="hv")+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=YNP.W.smooth), 
	            stat="identity", direction="hv")+
	  annotate("text", x=105, y=0.025, label="YNP West herd", 
	           hjust=0, size=2.6)+
	  annotate("text", x=105, y=0.024, label="(3 week smooth)[8]", 
	           hjust=0, size=2.6)+
	  annotate("text", x=115, y=0.020, label="YNP West herd", 
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  annotate("text", x=115, y=0.019, label="(sample data)[7]", 
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  xlab("Day-of-year")+
	  theme(axis.line.x = element_line(colour = "grey"),
	        axis.line.y = element_line(colour = "grey"),
	        panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(),
	        panel.border = element_rect(colour = "grey", fill=NA),
	        panel.background = element_blank(),
	        axis.title.y=element_blank())+
	  scale_x_continuous(breaks=c(30,92,153,214), expand=c(0.01,0),
	                     labels=c(181,243,304,365), limits=c(0,245))+
	  scale_y_continuous(expand=c(0.02,0))
	
	# Plot of fetal data for the Niobrara, Custer Park, and Wind Cave herds.
	Assort.plot <- ggplot()+
	  annotate("text", x=pl.m.xpos, y=max(plotdata.fet$Assorted.prob)*1.09, 
	           label=pl.m.names, size=2.7)+
	  annotate("rect", xmin = pl.m.x1, xmax = pl.m.x2, 
	           ymin=max(plotdata.fet$Assorted.prob)*1.02, 
	           ymax=max(plotdata.fet$Assorted.prob)*1.15, alpha = 0.250)+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=Assorted.prob),
	            stat="identity", color="red", alpha=0.7, direction="hv")+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=Assort.smooth), 
	            stat="identity", direction="hv")+
	  annotate("text", x=95, y=0.046, label="Niobrara, Custer State Park,", hjust=0, size=2.6)+
	  annotate("text", x=95, y=0.044, label="and Wind Cave National Park herds", hjust=0, size=2.6)+
	  annotate("text", x=95, y=0.042, label="(3 week smooth)[10]", hjust=0, size=2.6)+
	  annotate("text", x=105, y=0.026, label="Niobrara, Custer State Park,", 
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  annotate("text", x=105, y=0.024, label="and Wind cave National Park herds", 
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  annotate("text", x=105, y=0.022, label="(sample data)[9]", 
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  xlab("Day-of-year")+ylab("p(conception)")+
	  theme(axis.line.x = element_line(colour = "grey"),
	        axis.line.y = element_line(colour = "grey"),
	        panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(),
	        panel.border = element_rect(colour = "grey", fill=NA),
	        panel.background = element_blank())+
	  scale_x_continuous(breaks=c(30,92,153,214), expand=c(0.01,0),
	                     labels=c(181,243,304,365), limits=c(0,245))+
	  scale_y_continuous(expand=c(0.02,0))
	
	# Plot of Niobrara bull fights data.
	Niob.plot <- ggplot()+
	  annotate("text", x=pl.m.xpos, y=max(plotdata.prox$Niob.prob)*1.09, 
	           label=pl.m.names, size=2.7)+
	  annotate("rect", xmin = pl.m.x1, xmax = pl.m.x2, 
	           ymin=max(plotdata.prox$Niob.prob)*1.02, 
	           ymax=max(plotdata.prox$Niob.prob)*1.15, alpha = 0.250)+
	  geom_step(data=plotdata.prox, aes(x=Days.plot, y=Niob.prob),
	            stat="identity", color="red", alpha=0.7, direction="hv")+
	  geom_step(data=plotdata.prox, aes(x=Days.plot, y=Niob.smooth), 
	            stat="identity", direction="hv")+
	  annotate("text", x=115, y=0.085, label="Niobrara bull fights", hjust=0, size=2.6)+
	  annotate("text", x=115, y=0.081, label="(3 week smooth)[12]", hjust=0, size=2.6)+
	  annotate("text", x=115, y=0.059, label="Niobrara bull fights", 
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  annotate("text", x=115, y=0.055, label="(sample data)[11]", 
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  xlab("Day-of-year")+
	  theme(axis.line.x = element_line(colour = "grey"),
	        axis.line.y = element_line(colour = "grey"),
	        panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(),
	        panel.border = element_rect(colour = "grey", fill=NA),
	        panel.background = element_blank(),
	        axis.title.y=element_blank())+
	  scale_x_continuous(breaks=c(30,92,153,214), expand=c(0.01,0),
	                     labels=c(181,243,304,365), limits=c(0,245))+
	  scale_y_continuous(expand=c(0.02,0))
	
	# Plot of National Bison Range copulations data.
	Nbr.plot <- ggplot()+
	  annotate("text", x=pl.m.xpos, y=max(plotdata.prox$NBR.prob)*1.09, 
	           label=pl.m.names, size=2.7)+
	  annotate("rect", xmin = pl.m.x1, xmax = pl.m.x2, 
	           ymin=max(plotdata.prox$NBR.prob)*1.02, 
	           ymax=max(plotdata.prox$NBR.prob)*1.15, alpha = 0.250)+
	  geom_step(data=plotdata.prox, aes(x=Days.plot, y=NBR.prob),
	            stat="identity", color="red", alpha=0.7, direction="hv")+
	  geom_step(data=plotdata.prox, aes(x=Days.plot, y=NBR.smooth), 
	            stat="identity", direction="hv")+
	  annotate("text", x=90, y=0.128, label="National Bison Range copulations",hjust=0, size=2.6)+
	  annotate("text", x=90, y=0.122, label="(3 week smooth)[14]",hjust=0, size=2.6)+
	  annotate("text", x=90, y=0.082, label="National Bison Range copulations",
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  annotate("text", x=90, y=0.076, label="(sample data)[13]",
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  xlab("Day-of-year")+
	  theme(axis.line.x = element_line(colour = "grey"),
	        axis.line.y = element_line(colour = "grey"),
	        panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(),
	        panel.border = element_rect(colour = "grey", fill=NA),
	        panel.background = element_blank(),
	        axis.title.y=element_blank())+
	  scale_x_continuous(breaks=c(30,92,153,214), expand=c(0.01,0),
	                     labels=c(181,243,304,365), limits=c(0,245))+
	  scale_y_continuous(expand=c(0.02,0))
	
	# Plot of aggregated fetal data from the YNP, Custer Park, Niobrara, and
	# Wind Cave herds.
	Agg.plot <- ggplot()+
	  annotate("text", x=pl.m.xpos, y=max(plotdata.fet$Agg.ft.prob)*1.09, 
	           label=pl.m.names, size=2.7)+
	  annotate("rect", xmin = pl.m.x1, xmax = pl.m.x2, 
	           ymin=max(plotdata.fet$Agg.ft.prob)*1.02, 
	           ymax=max(plotdata.fet$Agg.ft.prob)*1.15, alpha = 0.250)+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=Agg.ft.prob),
	            stat="identity", color="red", alpha=0.7, direction="hv")+
	  geom_step(data=plotdata.fet, aes(x=Days.plot, y=AggF.smooth),
	            stat="identity", direction="hv")+
	  annotate("text", x=90, y=0.033, label="Aggregate herds", 
	           hjust=0, size=2.6)+
	  annotate("text", x=90, y=0.0315, label="(3 week smooth)[2]",
	           hjust=0, size=2.6)+
	  annotate("text", x=90, y=0.0255, label="Aggregate herds", 
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  annotate("text", x=90, y=0.024, label="(sample data)[1]", 
	           color="red", alpha=0.8, hjust=0, size=2.6)+
	  xlab("")+ylab("p(conception)")+
	  theme(axis.line.x = element_line(colour = "grey"),
	        axis.line.y = element_line(colour = "grey"),
	        panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(),
	        panel.border = element_rect(colour = "grey", fill=NA),
	        panel.background = element_blank())+
	  scale_x_continuous(breaks=c(30,92,153,214), expand=c(0.01,0),
	                     labels=c(181,243,304,365), limits=c(0,245))+
	  scale_y_continuous(expand=c(0.02,0))
	
	# Plot of reference information for all data.	
	Info.plot <- ggplot()+
	  annotate("text", x=0.50, y=0.98, label="Data References", size=3.5)+
	  annotate("text", x=0.01, y=0.90, label="YNP fetal data", size=3.2, hjust=0)+
	  annotate("text", x=0.01, y=0.85, 
	           label="Gogan PJP, Podruzny KM, Olexa EM, Pac HI, Frey KL", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.81, 
	           label="(2005) Yellowstone Bison Fetal Development and", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.77, 
	           label="Phenology of Parturition. Journal of Wildlife", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.73, 
	           label="Management 69, 1716-1730.", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.63, label="Niobrara, Custer Park, Wind Cave fetal data", 
	           size=3.2, hjust=0)+
	  annotate("text", x=0.01, y=0.58, 
	           label="Haugen AO (1974) Reproduction in the Plains Bison.", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.54, 
	           label="Iowa State Journal of Research 49, 1-8.", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.44, label="Niobrara bull fights data", 
	           size=3.2, hjust=0)+
	  annotate("text", x=0.01, y=0.39, 
	           label="Wolff JO (1998) Breeding Strategies, Mate Choice and", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.35, 
	           label="Reproductive Success in American Bison. Oikos 83,", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.31, 
	           label="529-544.", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.21, label="National Bison Range copulations data", 
	           size=3.2, hjust=0)+
	  annotate("text", x=0.01, y=0.16, 
	           label="Lott DF (1981) Sexual Behavior and Intersexual Strategies", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.12, 
	           label="in American Bison. Zeitshrift fur Tierpsychologie 56,", 
	           size=2.7, hjust=0)+
	  annotate("text", x=0.01, y=0.08, 
	           label="97-114.", size=2.7, hjust=0)+
	  theme(text = element_blank(),
	        line = element_blank(),
	        title = element_blank(),
	        panel.background = element_rect(fill = "white"))+
	  scale_x_continuous(limits=c(0,1), expand=c(0.01,0))+
	  scale_y_continuous(limits=c(0,1), expand=c(0.02,0))
	
	# Plot the 8 figures as panels in one figure.
	grid.arrange(Agg.plot, YNP.sum.plot, YNP.N.plot, YNP.W.plot, 
	             Assort.plot, Niob.plot, Nbr.plot, Info.plot, ncol=4)
	
	# Prompt user to select a conception schedule. If the user does
	# not select a valid index (2-15), display an error message and
	# prompt the user to re-enter an index.
	cat("Calendar index (integer value): ")
	tf <- readline()
	while (tf != "1" && tf != "2" && tf != "3" &&tf != "4" && tf != "5" &&
	       tf != "6" && tf != "7" && tf != "8" && tf != "9" && tf != "10" &&
	       tf != "11" && tf != "12" && tf != "13" && tf != "14"){
	  cat("ERROR: INDEX MUST BE AN INTEGER BETWEEN 1 AND 14",
        "\nEnter a new calendar index (integer value): ")
	  tf <- readline()
	}
	
	# Convert the user-entered index into a numeric value, then use this
	# value to select a conception schedule from the plotdata data frame.
	# Assign this to a vector (conception.schedule.model). Display a 
	# message informing the user that the calendar was successfully
	# selected.
	tf <- as.numeric(tf)
	conception.schedule.model <- plotdata.fet[,tf]
	cat("Model conception calendar", tf, "successfully selected.\n\n")

	# Return the selected calendar from the function.
	return(conception.schedule.model)
	}

# FUNCTION: Combined analysis of user-selected elements. This function
# requires five arguments. The first argument (calendars) is a data
# frame of all the SODES generated from the main analysis. The second
# argument (months) is a vector of 365 month names. The third argument 
# (dates) is a vector of dates within each month. The fourth argument
# (ar) is a matrix of minimum and maximum gestation ages associated with
# each SODE. The fifth argument (SeCal) is a conception distribution. 
# The function plots the combined SODE and input SODEs, and returns a
# list containinga new entry corresponding to the combined SODE.
analysis.combined <- function(calendars, months, dates, ar, SeCal){
  
  # Prompt user to enter element indices for combined analysis.
  cat("** ANALYSIS: COMBINED ELEMENT SODE **\n\n")
  cat("Enter the comma-and-space-separated indices for the entries that you\n")
  cat("would like to use. [e.g., '1, 3, 5, 7, 2']\n")	
  intersection.indices.string <- readline()
  
  while (suppressWarnings(anyNA(as.numeric(unlist(strsplit(intersection.indices.string, split=", ")))))){
    cat("ERROR: values must be comma-and-space separated integers.",
        "\nPlease re-enter indices: ")
    intersection.indices.string <- readline()
  }
  
  # Convert entered indices from string to a numeric vector, round any non-integers to integers.
  intersection.indices.vect <- as.numeric(unlist(strsplit(intersection.indices.string, split=", ")))
  intersection.indices.vect <- unique(round(intersection.indices.vect))
  
  while (any(intersection.indices.vect < 1) | any(intersection.indices.vect > ncol(calendars)) |
         length(intersection.indices.vect) < 2){
    cat("ERROR: too few entries (requires > 1), or one or more values is outside \nthe range of entries (1-",
        ncol(calendars),"). Please re-enter indices: ",sep="")
    intersection.indices.string <- readline()
    while (suppressWarnings(anyNA(as.numeric(unlist(strsplit(intersection.indices.string, split=", ")))))){
      cat("ERROR: values must be comma-and-space separated integers.\nPlease re-enter indices")
      intersection.indices.string <- readline()
    }
    intersection.indices.vect <- as.numeric(unlist(strsplit(intersection.indices.string, split=", ")))
    intersection.indices.vect <- unique(round(intersection.indices.vect))
  }
  
  # Assign index name for the distribution that is to be calculated.
  dist.name <- paste("combined [entries ", intersection.indices.string, "]", sep="")
  
  # Created a matrix with 365 rows of zeros and columns reflecting the number of
  # entries that the user has selected.
  intersection.vectors <- matrix(0, nrow(calendars), length(intersection.indices.vect))
  
  # Created a matrix with to hold the gestation ages for the selected entries
  rangeofages <- matrix(0, length(intersection.indices.vect), 2)
  
  # For each matrix column of zeroes, replace with a vector of death date probabilities 
  # corresponding to the entry index specified by the user. This creates a matrix of prior
  # death date probabilities spanning a full calendar year. Store the corresponding age
  # ranges in ar.
  for (y in 1:length(intersection.indices.vect)){
    intersection.vectors[,y] <- calendars[,intersection.indices.vect[y]]
    rangeofages[y,] <- ar[intersection.indices.vect[y],]
  }
  
  # Store the maximum min age and minimum max age in corange ("combined range").
  # This is the widest age range consistent with all selected elements.
  corange <- c(max(rangeofages[,1]),min(rangeofages[,2]))
  
  # Create vector of days 365 bins long.
  Day <- rep(1:nrow(calendars))
  
  # If the user specified elements with non-overlapping ages, notify user
  # and set combined distribution to a uniform prob of 0. Set the plotting
  # data frame to reflect only zero probabilities across the year.
  if(corange[1]>corange[2]){
    
    cat("\nAge distributions do not overlap, no combined estimate possible.\n")
    combined <- rep(0,365)
    # Set maximum y-xis value for plotting
    YM <- max(intersection.vectors)
  }
  
  # If the user entered overlapping age ranges, obtain the combined SODE.
  else { 
    
    #######################################################################
    ######## estimate the distribution from the age range: start ##########
    #######################################################################
    
    # Create an empty vector of probabilities for the full reproductive cycle.
    DeathP <- rep(0, 579)
    
    # For each potential gestation age y in corange[1] to corange[2]:
    # reproduce the conception probability distribution in an interval
    # that is y gestation days advanced from the conception dates. These
    # are the death date probabilities. Finally, the loop sums probab-
    # ilities in each iteration. This is done for the full reproductive
    # cycle calendar.
    for (y in corange[1]:corange[2]){
      ymax <- y + length(SeCal) - 1
      DeathP[y:ymax] <- DeathP[y:ymax]+SeCal
    }
    
    # Assign the probabilities from the full reproductive cycle calendar to a
    # 365-day calendar.
    DeathP365 <- rep(0,365)
    DeathP365[152:365] <- DeathP[1:214] + DeathP[366:579]
    DeathP365[1:151] <- DeathP[215:365]
    
    # Standardize this 365-day calendar.
    combined <- DeathP365/sum(DeathP365)
    
    #######################################################################
    ######## estimate the distribution from the age range: end ##########
    #######################################################################
    
    # Create empty vectors to store up to 365 probability masses. These
    # capture discrete intervals when the combined SODE is discontinuous across
    # the calendar year. The vectors store the upper and lower bounds for
    # each interval.
    dist.lb <- rep(0, 365)
    dist.ub <- rep(0, 365)
    
    # Create numeric variables to track the number of date intervals.
    dist.lb.count <- 0
    dist.ub.count <- 0
    
    # For each day in the year, detect if the day represents the beginning
    # or ending of a combined interval. This is accomplished by examining
    # the location of combined interval 0 probs that are adjacent to >0 probs.
    for (u in 1:364){
      if (combined[u] == 0 & combined[u+1] > 0){
        dist.lb.count <- dist.lb.count + 1
        dist.lb[dist.lb.count] <- u+1
      }
      else if (dist.lb.count > 0 & combined[u+1] == 0 & combined[u] > 0){
        dist.ub.count <- dist.ub.count + 1
        dist.ub[dist.ub.count] <- u
      }
      else if (u == 1 & combined[u] > 0 & combined[365] == 0){
        dist.lb.count <- dist.lb.count + 1
        dist.lb[dist.lb.count] <- u
      }
      else if (u == 365 & combined[u] > 0 & combined[1] == 0){
        dist.ub.count <- dist.ub.count + 1
        dist.ub[dist.ub.count] <- u
      }
    }
    
    # If an interval overlaps Dec 31 - Jan 1, the preceding for loop will
    # not have found the position of upper bound for the final interval.
    # For this scenario, this if statement finds this final upper bound.
    if (max(which(combined > 0)) == 365 & min(which(combined > 0)) == 1){
      dist.ub.count <- dist.ub.count+1
      dist.ub[dist.ub.count] <- min(which(combined == 0)) - 1
    }
    
    # If the combined SODE is continuously distributed (i.e., there is only one
    # upper and one lower bound), print the date range for the user.
    if (dist.lb.count == 1){
      cat("\nEntire combined SODE is distributed from ", months[dist.lb[1]], " ", 
          dates[dist.lb[1]], " to ", months[dist.ub[1]], " ", dates[dist.ub[1]], 
          ".", sep="")
    }
    
    # If the combined SODE is discontinuously distributed (i.e., there are multiple
    # upper and lower bounds), but none of intervals overlap Dec 31 - Jan 1,
    # display the date ranges and their associated probability masses to the
    # user.
    else if (max(which(combined > 0)) != 365 & min(which(combined > 0)) != 1){
      cat("\nCombined SODE is distributed across ", dist.lb.count,
          " intervals.", sep="")
      
      # This for loop prints the date ranges for each interval.
      for (v in 1:dist.lb.count){
        cat("\n", sum(combined[dist.lb[v]:dist.ub[v]]), " of the combined SODE ",
            "is distributed from ", months[dist.lb[v]], " ", dates[dist.lb[v]],
            " to ", months[dist.ub[v]], " ", dates[dist.ub[v]], ".", sep="")
      }
    }
    
    # If the combined SODE is discontinuously distributed (i.e., there are multiple
    # upper and lower bounds), and one interval overlaps Dec 31 - Jan 1,
    # display the date ranges and their associated probability masses to the
    # user.
    else {
      cat("\nCombined SODE is distributed across ", dist.lb.count,
          " intervals.", sep="")
      forcount <- dist.lb.count-1
      
      # This for loop prints the date ranges for each interval. It skips the
      # final interval since the vector positions in the last interval are
      # in reverse ordinal order (ie, it would attempt to sum probabilities
      # beginning at a high number and ending at a lower number).
      for (v in 1:forcount){
        cat("\n", sum(combined[dist.lb[v]:dist.ub[v]]), " of the combined SODE ",
            "is distributed from ", months[dist.lb[v]], " ", dates[dist.lb[v]],
            " to ", months[dist.ub[v]], " ", dates[dist.ub[v]], ".", sep="")
      }
      
      # These statements sum the probabilities in each portion of the final
      # date interval and print the results in a statement similar to the one
      # contained in the preceding for loop.
      final.sum <- sum(combined[dist.lb[dist.lb.count]:365])+
        sum(combined[1:dist.ub[dist.ub.count]])
      cat("\n", final.sum, " of the combined SODE ",
          "is distributed from ", months[dist.lb[dist.lb.count]], " ", 
          dates[dist.lb[dist.lb.count]], " to ", months[dist.ub[dist.lb.count]], 
          " ", dates[dist.ub[dist.lb.count]], ".", sep="")
    }
    
    # Set maximum y-xis value for plotting
    YM <- max(combined)
  }
  
  # Create vectors for geometry that captures the combined distribution. Subtract
  # 0.5 from days vector; this centers the horizontal portions of step geometry over
  # each each day on the x-axis.
  comb.probs.dbl <- rep(0, 731)
  comb.days.dbl <- rep(0, 731)
  comb.days.dbl[731] <- 365.5
  for (p in 1:length(combined)){
    comb.probs.dbl[2*p+1] <- combined[p]
    comb.probs.dbl[2*p] <- combined[p]
    comb.days.dbl[2*p-1] <- p - 0.5
    comb.days.dbl[2*p] <- p - 0.5
  }
  
  # Assign geometry vectors to a data frame for plotting purposes.
  comb.dbl.df <- data.frame(comb.days.dbl, comb.probs.dbl, stringsAsFactors=FALSE)
  
  # Create vector of days that centers the probability over each day for plotting purposes.
  DayOffset <- Day - 0.5
  
  # Collapse original SODES and define each row-wise probability
  # by the day in which it occurs.
  calendarsmelt <- melt(data.frame(intersection.vectors, DayOffset), id.vars="DayOffset")
  
  # Format entry variable for plot legend.
  colnames(calendarsmelt)[2] <- "Entry"
  calendarsmelt$Entry <- substring(calendarsmelt$Entry, 2)
  calendarsmelt$Entry <- as.character(intersection.indices.vect[as.numeric(calendarsmelt$Entry)])
  
  # Create plot that spans a 365 day year. Demarcate months with text and semi-
  # transparent grey recctangles. Plot prior distributions as step geometry
  # defined by unique colors. Plot the combined distribution as a semi-
  # transparent polygon.
  combined.plot <- ggplot()+
    annotate("rect", xmin = p.x1, xmax = p.x2, ymin=YM*1.02, 
             ymax=YM*1.15, alpha = 0.25)+
    annotate("text", x=p.month.xpos, y=YM*1.09, 
             label=p.month.name, size=3)+
    geom_step(data=calendarsmelt, aes(x=DayOffset, y=value, color=Entry),
              stat="identity", direction="hv", alpha=0.9)+
    geom_ribbon(data=comb.dbl.df, aes(x=comb.days.dbl, ymin=0, 
                                      ymax=comb.probs.dbl), fill="black", alpha=0.2)+
    xlab("Day-of-year")+ylab("p(deposition)")+
    theme(axis.line.x = element_line(colour = "grey"),
          axis.line.y = element_line(colour = "grey"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill=NA),
          panel.background = element_blank())+
    scale_y_continuous(limits=c(0,YM*1.15), expand=c(0.02,0))+
    scale_x_continuous(breaks=c(31, 90, 151, 212, 273, 334), expand=c(0,0))
  
  # Display the combined plot.
  plot(combined.plot)
  
  # Create a list with the the name of the combined SODE (defined at
  # the beginning of the function) and the combined SODE itself. This
  # list therefore contains a string and a numeric vector.
  dist.info <- list(dist.name, combined)
  
  # Return the list.
  return(dist.info)
  
}


# FUNCTION: Compares user selected death-date distributions against
# user-selected intervals. This function takes four arguments.
# The first argument is a data frame of death date distributions.
# The next three arguments are months, 1:365 days, and the dates
# within each month. This function prints probabilities for each
# distribution for a given interval and plots the results. It 
# does not return an object.
analysis.interval <- function(calendars, months, days, date){

	# Prompt user to enter elements for death date intersection analysis.
	cat("** ANALYSIS: DATE INTERVALS AND SODE OVERLAPS**\n\n")
	cat("Enter comma-and-space-separated indices for the SODEs that\n")
	cat("you would like to include. [e.g., '1, 3, 8, 26, 15']\n")
	intersection.indices.string <- readline()

	# Split the user-defined entries into a vector and ensure that they are numeric
	# values. If they are not numeric, display a warning and prompt the user to re-
	# enter the list of entries.
	while (suppressWarnings(anyNA(as.numeric(unlist(strsplit(intersection.indices.string, 
	       split=", ")))))){
	  cat("ERROR: values must be comma-and-space separated integers.",
	      "\nPlease re-enter indices: ")
	  intersection.indices.string <- readline()
	}
	
	# Convert entered indices from string to a numeric vector, round any non-integers to integers.
	intersection.indices.vect <- as.numeric(unlist(strsplit(intersection.indices.string, split=", ")))
	intersection.indices.vect <- round(intersection.indices.vect)
	
	# Ensure that the user-entered values correspond to indices in the death-date distributions
	# in the calendars data frame. If the values do not correspond to the data frame, display an
	# error and prompt the user to re-enter values. These nested while loops will only break
	# when the user has entered numeric values that correspond to the calendars data frame.
	while (any(intersection.indices.vect < 1) | any(intersection.indices.vect > ncol(calendars))){
	  cat("ERROR: one or more values is outside the range of entries (1-",ncol(calendars),").", 
	      "\nPlease re-enter indices.", sep="")
	  intersection.indices.string <- readline()
	  # Check re-entered values to ensure that they are all numeric.
	  while (suppressWarnings(anyNA(as.numeric(unlist(strsplit(intersection.indices.string, split=", ")))))){
	    cat("ERROR: values must be comma-and-space separated integers.",
	        "Please re-enter indices: ")
	    intersection.indices.string <- readline()
	  }
	  # Convert re-entered indices from string to a numeric vector, round any non-integers to integers.
	  intersection.indices.vect <- as.numeric(unlist(strsplit(intersection.indices.string, split=", ")))
	  intersection.indices.vect <- round(intersection.indices.vect)
	}

	# Create a matrix with 365 rows of zeros and columns reflecting the number of
	# entries that the user has selected.
	intersection.vectors <- data.frame(matrix(0, nrow(calendars), length(intersection.indices.vect)))

	# Assign a death date distribution to each column in the matrix of zeros.
	for (y in 1:length(intersection.indices.vect)){
			intersection.vectors[,y] <- calendars[,intersection.indices.vect[y]]
		}

	# Create a data frame for one calendar year containing dates, months, and indices
	# for all 365 days.
	interface.calendar <- data.frame(months, date, stringsAsFactors=FALSE)

	# Prompt user to select a date range for the interval analysis. This displays
	# a full calendar year with corresponding indices from which to choose. the
	# index interval is stored in the variable cinterval.str.
	cat("\n")
	print(interface.calendar)
	cat("\n\nEnter comma-and-space-separated indices for the interval dates.",
	"\n[e.g., '122, 215' for May 2 to Aug 3; '334, 154' for Nov 31 to Jun 2]",
	"\nIf you are only interested in overlaps between distributions without",
	"\nrespect to a specific time interal, enter '1, 365'.\n")
	cinterval.str <- readline()
	
	# Ensure that user has entered numeric values. If non-numeric values are 
	# present, display error and prompt user to re-enter values.
	while (suppressWarnings(anyNA(as.numeric(unlist(strsplit(cinterval.str, split=", ")))))){
	  cat("ERROR: values must be comma-and-space separated integers.",
	      "\nPlease re-enter indices: ")
	  cinterval.str <- readline()
	}
	
	# Convert entered indices from string to a numeric vector, round any non-integers to integers.
	calendar.interval <- as.numeric(unlist(strsplit(cinterval.str, split=", ")))
	calendar.interval <- round(calendar.interval)
	
	# Ensure that there are only two numeric values between 1 and 365. These nested
	# while loops will only breaks when two numeric values between 1 and 365 have 
	# been assigned to cinterval.str (calendar.interval).
	while (any(calendar.interval < 1) | any(calendar.interval > 365 | length(calendar.interval) != 2)){
	  cat("ERROR: one or more date indices is outside the calendar range (1-365),", 
        "\nor you have entered more or less than two date indices.",
	      "\nPlease re-enter indices: ")
	      cinterval.str <- readline()
	  # In the case of re-entered values, re-check to ensure that they are numeric. 
	  while (suppressWarnings(anyNA(as.numeric(unlist(strsplit(cinterval.str, split=", ")))))){
	    cat("ERROR: values must be comma-and-space separated integers.",
	        "\nPlease re-enter indices: ")
	    cinterval.str <- readline()
	  }
	  # In the case of re-entered values, convert entered indices from string to a numeric vector, 
	  # round any non-integers to integers.
	  calendar.interval <- as.numeric(unlist(strsplit(cinterval.str, split=", ")))
	  calendar.interval <- round(calendar.interval)
	}

	# Initialize day-wise vectors for the product of shared probabilities between
	# death date distributions, the minimum probability between death date 
	# distributions, and the maximum probability between death date distributions.
	prod.probs <- rep(0, 365)	
	min.probs <- rep(0, 365)
	max.probs <- rep(0, 365)

	# Calculate values for product, minimum, and maximum probability vectors.
	# If date interval does not overlap the turn of a year (ie, the dates do
	# not move forward in time from Dec 31 to Jan 1), calculate these
	# probabilities over the date interval.	
	if (calendar.interval[1] < calendar.interval[2]){
		for (n in calendar.interval[1]:calendar.interval[2]){
			prod.probs[n] <- prod(intersection.vectors[n,])
			min.probs[n] <- min(intersection.vectors[n,])
			max.probs[n] <- max(intersection.vectors[n,])
			}

		# Intialize vector with bins for each distribution; this vector
		# will contain interval probabilities for each distribution.
		individual.probs <- rep(0, ncol(intersection.vectors))

		# For each distribution, sum the probabilities within the date interval.
		for (d in 1:length(individual.probs)){
			individual.probs[d] <- sum(intersection.vectors[calendar.interval[1]:calendar.interval[2],d])
			}
		}

		# If the date interval overlaps the turn of a year, calculate these
		# probabilties over two segments of time (pre and post new year).
		else {
			for (e in calendar.interval[1]:365){
				prod.probs[e] <- prod(intersection.vectors[e,])
				min.probs[e] <- min(intersection.vectors[e,])
				max.probs[e] <- max(intersection.vectors[e,])
				}
			for (f in 1:calendar.interval[2]){
				prod.probs[f] <- prod(intersection.vectors[f,])
				min.probs[f] <- min(intersection.vectors[f,])
				max.probs[f] <- max(intersection.vectors[f,])
				}

			# Intialize vector with bins for each distribution; this vector
			# will contain interval probabilities for each distribution.
			individual.probs <- rep(0, ncol(intersection.vectors))

			# For each distribution, sum the probabilities within the 
			# date interval segments.
			for (j in 1:length(individual.probs)){
				individual.probs[j] <- sum(intersection.vectors[calendar.interval[1]:365,j])
				individual.probs[j] <- individual.probs[j] + sum(intersection.vectors[1:calendar.interval[2],j])
				}
			}

	# Create vectors for geometry that capture the shared overlap between all
	# distributions within the interval (min.probs) as well as the probability
	# for each individual distribution within the date interval (max.probs).
	# Subtract 0.5 from each day to center probabilities over each day in the
	# step geometry plots. This centers the horizontal step portion of the
	# geometry over the x-axis day value (as opposed to the vertical step).
	min.probs.dbl <- rep(0, 731)
	max.probs.dbl <- rep(0, 731)
	min.days.dbl <- rep(0, 731)
	min.days.dbl[731] <- 365.5	
	for (h in 1:length(min.probs)){
		min.probs.dbl[2*h] <- min.probs[h]
		min.probs.dbl[(2*h)+1] <- min.probs[h]
		max.probs.dbl[2*h] <- max.probs[h]
		max.probs.dbl[(2*h)+1] <- max.probs[h]
		min.days.dbl[2*h] <- h-0.5
		min.days.dbl[(2*h)-1] <- h-0.5
		}

	# Assign geometry vectors to data frame for plotting purposes.
	min.df <- data.frame(min.probs.dbl, max.probs.dbl, min.days.dbl, stringsAsFactors=FALSE)

	# If the user did not create a specific interval (ie, the whole year is specified),
	# and only one distribution was specified, notify the user.
	if (calendar.interval[1] == 1 & calendar.interval[2] == 365 & 
	         length(intersection.indices.vect) == 1){
		cat("\nNo specific hypothesized date interval and only one element",
		    "\nselected. No probabilitistic statement.\n\n")
		}

	# If the user did not create a specific interval (ie, the whole year is specified),
	# and multiple distributions were selected, print the probability that they overlapped.
	else if (calendar.interval[1] == 1 & calendar.interval[2] == 365){
		cat("\nThe probability that all elements were deposited on the same date \nis ", 
		sum(prod.probs), ".\n\n", sep="")
		min.probs.df <- data.frame(days, min.probs, stringsAsFactors=FALSE)
		}

	# If the user specified a date interval and multiple death date distributions, display
	# the probability that the distributions share a death date within the interval as well
	# as the probability that all distributions fall within the date interval.
	else if (length(intersection.indices.vect) > 1){ 
		cat("\nThe probability that all elements were deposited on the same",
		"\ndate within the interval ", months[calendar.interval[1]], " ",
		date[calendar.interval[1]], " - ", months[calendar.interval[2]], " ",
		date[calendar.interval[2]], " is ", sum(prod.probs), ".\n", sep="")
		cat("The probability that all elements were deposited",
		"\nwithin the interval ", months[calendar.interval[1]], " ",
		date[calendar.interval[1]], " - ", months[calendar.interval[2]], " ",
		date[calendar.interval[2]], " is ", prod(individual.probs), ".\n", sep="")
		min.probs.df <- data.frame(days, min.probs, stringsAsFactors=FALSE)
		}

	# If the user specified a date interval and one death date distribution, display
	# the probability that the distribution's death date falls within the interval.
	else if (length(intersection.indices.vect) == 1){
		cat("\nThe probability that the element was deposited within\n")
		cat(months[calendar.interval[1]], " ", date[calendar.interval[1]], " - ", 
		months[calendar.interval[2]], " ", date[calendar.interval[2]], 
		" is ", sum(prod.probs), ".\n", sep="")
		min.probs.df <- data.frame(days, min.probs, stringsAsFactors=FALSE)
		}
	
	# Create vector of days to center probabilities over days, for plotting purposes.
	# this centers the horizontal step geometry over the x-axis day values.
	daysoffset <- days-0.5
  
	# Collapse distributions and offset days into calendarsmelt
	calendarsmelt <- melt(data.frame(intersection.vectors, daysoffset), id.vars="daysoffset")

	# Ensure that varible designation displays as entry numbers for plotting.
	colnames(calendarsmelt)[2] <- "Entry"
	calendarsmelt$Entry <- substring(calendarsmelt$Entry, 2)
	if (suppressWarnings(anyNA(as.numeric(as.character(calendarsmelt$Entry))))){
	  calendarsmelt$Entry <- "1"
	  }
	calendarsmelt$Entry <- as.character(intersection.indices.vect[as.numeric(calendarsmelt$Entry)])

	# This else if framework creates a plot dependent on the number of distributions
	# and whether the date interval ooverlaps the turn of a year (ie, moving forward
	# in time from Dec 31 to Jan 1).Death date distributions are symbolized by step 
	# geometry and intervals are symbolized by semi-transparent rectangles. 
	# Intersecting probability regions are symbolized by semi-opaque polygons.
	
	# Define variables for Y-axis heights for geometry and text in plots.
	YMaG <- max(apply(intersection.vectors, 2, max))*1.15
	YMaG2 <- max(apply(intersection.vectors, 2, max))*1.02
	YMaT <- max(apply(intersection.vectors, 2, max))*1.09
	YMaH <- max(apply(intersection.vectors, 2, max))
	
	# First plot is for an unspecified interval (ie, spanning a whole calendar year)
	# and only one death date distribution. In this case, no real analysis has been
	# done, but the input distribution is still plotted for the user.
	if (calendar.interval[1] == 1 & calendar.interval[2] == 365 & 
	    length(intersection.indices.vect) == 1){
	  interval.plot <- ggplot()+
	    annotate("rect", xmin = p.x1, xmax = p.x2, ymin = YMaG2, 
	             ymax = YMaG, alpha = 0.25)+
	    annotate("text", x=p.month.xpos, y=YMaT, 
	             label=p.month.name, size=3)+
	    geom_step(data=calendarsmelt, aes(x=daysoffset, y=value, color=Entry),
	              stat="identity", direction="hv", alpha=0.9)+
	    xlab("Day-of-year")+ylab("p(deposition)")+
	    theme(axis.line.x = element_line(colour = "grey"),
	          axis.line.y = element_line(colour = "grey"),
	          panel.grid.major = element_blank(),
	          panel.grid.minor = element_blank(),
	          panel.border = element_rect(colour = "grey", fill=NA),
	          panel.background = element_blank())+
	    scale_y_continuous(limits=c(0,YMaG), expand=c(0.02,0))+
	    scale_x_continuous(breaks=c(31, 90, 151, 212, 273, 334), expand=c(0,0))
	}
	
	# Second plot is for an unspecified interval (ie, spanning a whole calendar year)
	# and multiple death date distributions. In this case, only the intersecting region
	# is indicated by a semi-transparent polygon.
	else if (calendar.interval[1] == 1 & calendar.interval[2] == 365 & 
	         length(intersection.indices.vect) > 1){
	  interval.plot <- ggplot()+
	    annotate("rect", xmin = p.x1, xmax = p.x2, ymin = YMaG2, 
	             ymax = YMaG, alpha = 0.25)+
	    annotate("text", x=p.month.xpos, y=YMaT, 
	             label=p.month.name, size=3)+
	    geom_step(data=calendarsmelt, aes(x=daysoffset, y=value, color=Entry),
	              stat="identity", direction="hv", alpha=0.9)+
	    geom_ribbon(data=min.df, aes(x=min.days.dbl, ymin=0, 
	                                 ymax=min.probs.dbl), fill="black", alpha=0.22)+
	    xlab("Day-of-year")+ylab("p(deposition)")+
	    theme(axis.line.x = element_line(colour = "grey"),
	          axis.line.y = element_line(colour = "grey"),
	          panel.grid.major = element_blank(),
	          panel.grid.minor = element_blank(),
	          panel.border = element_rect(colour = "grey", fill=NA),
	          panel.background = element_blank())+
	    scale_y_continuous(limits=c(0,YMaG), expand=c(0.02,0))+
	    scale_x_continuous(breaks=c(31, 90, 151, 212, 273, 334), expand=c(0,0))
	}
	
	# Third plot is for a specified interval and multiple death date distributions. 
	# In this case, the interval does not span the turn of a year (ie, does not
	# move forward from dec 31 through Jan 1). The interval is indicated with a 
	# semi-transparent rectangle, and intersecting probabilities are marked by semi-
	# opaque polygons.
	else if (calendar.interval[1] < calendar.interval[2] & 
	         length(intersection.indices.vect) > 1){
	  interval.plot <- ggplot()+
	    annotate("rect", xmin = calendar.interval[1]-0.5, 
	             xmax = calendar.interval[2]+0.5, 
	             ymin = 0, ymax = YMaH, fill="black", alpha = 0.08)+
	    annotate("rect", xmin = p.x1, xmax = p.x2, ymin = YMaG2, 
	             ymax = YMaG, alpha = 0.25)+
	    annotate("text", x=p.month.xpos, y=YMaT, 
	             label=p.month.name, size=3)+
	    geom_step(data=calendarsmelt, aes(x=daysoffset, y=value, 
	                                      color=Entry), stat="identity", direction="hv", 
	              alpha=0.9)+
	    geom_ribbon(data=min.df, aes(x=min.days.dbl, ymin=0, 
	                                 ymax=max.probs.dbl), fill="black", alpha=0.22)+
	    xlab("Day-of-year")+ylab("p(deposition)")+
	    theme(axis.line.x = element_line(colour = "grey"),
	          axis.line.y = element_line(colour = "grey"),
	          panel.grid.major = element_blank(),
	          panel.grid.minor = element_blank(),
	          panel.border = element_rect(colour = "grey", fill=NA),
	          panel.background = element_blank())+
	    scale_y_continuous(limits=c(0,YMaG), expand=c(0.02,0))+
	    scale_x_continuous(breaks=c(31, 90, 151, 212, 273, 334), 
	                       expand=c(0,0))
	}
	
	# Fourth plot is for a specified interval and a single death date distribution. 
	# In this case, the interval does not span the turn of a year (ie, does not
	# move forward from dec 31 through Jan 1). The interval is indicated with a 
	# semi-transparent rectangle, and intersecting probability region is marked by 
	# a semi-opaque polygon.
	else if (calendar.interval[1] < calendar.interval[2] & 
	         length(intersection.indices.vect) == 1){
	  interval.plot <- ggplot()+
	    annotate("rect", xmin = calendar.interval[1]-0.5, 
	             xmax = calendar.interval[2]+0.5, ymin = 0, 
	             ymax = YMaH, fill="black", alpha = 0.08)+
	    annotate("rect", xmin = p.x1, xmax = p.x2, ymin = YMaG2, 
	             ymax = YMaG, alpha = 0.25)+
	    annotate("text", x=p.month.xpos, y=YMaT, 
	             label=p.month.name, size=3)+
	    geom_step(data=calendarsmelt, aes(x=daysoffset, y=value, 
	                                      color=Entry), stat="identity", direction="hv", 
	              alpha=0.9)+
	    geom_ribbon(data=min.df, aes(x=min.days.dbl, ymin=0, 
	                                 ymax=min.probs.dbl), fill="black", alpha=0.22)+
	    xlab("Day-of-year")+ylab("p(deposition)")+
	    theme(axis.line.x = element_line(colour = "grey"),
	          axis.line.y = element_line(colour = "grey"),
	          panel.grid.major = element_blank(),
	          panel.grid.minor = element_blank(),
	          panel.border = element_rect(colour = "grey", fill=NA),
	          panel.background = element_blank())+
	    scale_y_continuous(limits=c(0,YMaG), expand=c(0.02,0))+
	    scale_x_continuous(breaks=c(31, 90, 151, 212, 273, 334), 
	                       expand=c(0,0))
	}
	# Fifth plot is for a specified interval and a single death date distribution. 
	# In this case, the interval spans the turn of a year (ie, moves forward from 
	# Dec 31 through Jan 1). The interval segments are indicated with semi-transparent
	# rectangles, and intersecting probability region is marked by a semi-opaque
	# polygon.
	else if (length(intersection.indices.vect) == 1){
	  interval.plot <- ggplot()+
	    annotate("rect", xmin = calendar.interval[1]-0.5, xmax = 365.5, 
	             ymin = 0, ymax = YMaH, fill="black", alpha = 0.08)+
	    annotate("rect", xmin = 0.5, xmax = calendar.interval[2]+0.5, 
	             ymin = 0, ymax = YMaH, fill="black", alpha = 0.08)+
	    annotate("rect", xmin = p.x1, xmax = p.x2, ymin = YMaG2, 
	             ymax = YMaG, alpha = 0.25)+
	    annotate("text", x=p.month.xpos, y=YMaT, 
	             label=p.month.name, size=3)+
	    geom_step(data=calendarsmelt, aes(x=daysoffset, y=value, 
	                                      color=Entry), stat="identity", direction="hv", 
	              alpha=0.9)+
	    geom_ribbon(data=min.df, aes(x=min.days.dbl, ymin=0, 
	                                 ymax=min.probs.dbl), fill="black", alpha=0.22)+
	    xlab("Day-of-year")+ylab("p(deposition)")+
	    theme(axis.line.x = element_line(colour = "grey"),
	          axis.line.y = element_line(colour = "grey"),
	          panel.grid.major = element_blank(),
	          panel.grid.minor = element_blank(),
	          panel.border = element_rect(colour = "grey", fill=NA),
	          panel.background = element_blank())+
	    scale_y_continuous(limits=c(0,YMaG), expand=c(0.02,0))+
	    scale_x_continuous(breaks=c(31, 90, 151, 212, 273, 334), 
	                       expand=c(0,0))
	}
	
	# Sixth plot is for a specified interval and multiple death date distributions. 
	# In this case, the interval spans the turn of a year (ie, moves forward from 
	# Dec 31 through Jan 1). The interval segments are indicated with semi-transparent
	# rectangles, and intersecting probability regions are marked by semi-opaque
	# parent polygons.
	else {
	  interval.plot <- ggplot()+
	    annotate("rect", xmin = calendar.interval[1]-0.5, xmax = 365.5, 
	             ymin = 0, ymax = YMaH, fill="black", alpha = 0.08)+
	    annotate("rect", xmin = 0.5, xmax = calendar.interval[2]+0.5, 
	             ymin = 0, ymax = YMaH, fill="black", alpha = 0.08)+
	    annotate("rect", xmin = p.x1, xmax = p.x2, ymin = YMaG2, 
	             ymax = YMaG, alpha = 0.25)+
	    annotate("text", x=p.month.xpos, y=YMaT, 
	             label=p.month.name, size=3)+
	    geom_step(data=calendarsmelt, aes(x=daysoffset, y=value, 
	                                      color=Entry), stat="identity", direction="hv", 
	              alpha=0.9)+
	    geom_ribbon(data=min.df, aes(x=min.days.dbl, ymin=0, 
	                                 ymax=max.probs.dbl), fill="black", alpha=0.22)+
	    xlab("Day-of-year")+ylab("p(deposition)")+
	    theme(axis.line.x = element_line(colour = "grey"),
	          axis.line.y = element_line(colour = "grey"),
	          panel.grid.major = element_blank(),
	          panel.grid.minor = element_blank(),
	          panel.border = element_rect(colour = "grey", fill=NA),
	          panel.background = element_blank())+
	    scale_y_continuous(limits=c(0,YMaG), expand=c(0.02,0))+
	    scale_x_continuous(breaks=c(31, 90, 151, 212, 273, 334), 
	                       expand=c(0,0))
	}
	
	# Display the plot that satisfies the "if else" framework.
	plot(interval.plot)
	
	}


# FUNCTION: Prompt user to enter or upload metric data. This function takes
# an argument specifying limits for possible metric values . It returns a 
# data frame with two columns, one specifying element types and another 
# specifying the metric value for each entered element.
entered.data <- function(sim.limits){
  
  # Prompt user to upload or manually enter data.
  cat("Would you like to manually enter metric values",
      "for each specimen ('m')\nor upload an existing",
      "csv table of elements with metric values ('u')? ")
  entry.choice <- readline()
  
  # Ensure that user has entered a valid character for manual/upload choice. 
  while(entry.choice != "m" && entry.choice != "u"){
    cat("ERROR: MUST ENTER 'm' OR 'u'",
        "\nPlease choose manual entry ('m') or csv upload ('u'): ")
    entry.choice <- readline()
  }
  
  # Guide user through manual entry.
  if (entry.choice == "m"){
  
	# Display text prompting user to enter how many entries will be made. Ensure that user
  # has entered a numeric value. If they have not entered a numeric value, display error
  # message and prompt for re-entry.
	cat("\nHow many specimens will be input (enter an integer)? ")
  ssizestr <- readline()
	while (is.na(suppressWarnings(as.numeric(ssizestr)))){
	    cat("ERROR: MUST BE A NON-NEGATIVE INTEGER VALUE",
	        "\nRe-enter number of specimens: ")
	    ssizestr <- readline()
	}
	# Store valid ssizestr string as numeric ssize.
	ssize <- as.numeric(ssizestr)

	# Initialize vectors for element name and metric value based on the
	# number of entries specified by the user.
	element.type <- vector(mode="character", length=ssize)
	metric.value <- vector(mode="numeric", length=ssize)

	# Combine intialized vectors into a data frame.
	dataset <- data.frame(element.type, metric.value, stringsAsFactors=FALSE)
	
	# Print message informing user of valid element type entries.
	cat("\nValid element types: 'radius, 'humerus', 'tibia', or 'femur'.",
	    "\nPress ENTER after each entry.\n")

	# For each entry, prompt the user to specify an element and a metric value.
	for (a in 1:ssize){
		cat(paste("\nWhat type of element is entry ", a, "? ", sep=""))
		rlprompt <- readline()
		# Ensure that entered element type is valid. If (while) it is not valid, display
		# error message and prompt user to re-enter value.
		while (rlprompt != "radius" && rlprompt != "humerus" &&
		       rlprompt != "tibia" && rlprompt != "femur"){
		  cat(paste("ERROR: ELEMENT TYPE MUST BE 'radius', 'humerus', 'tibia', OR 'femur'",
		      "\nEnter element type for entry ", a, ": ", sep=""))
		  rlprompt <- readline()
		}
		
		# If else statements that assign the simulated metric vectors for each
		# element based on the entered element.
		if (rlprompt == "radius"){
		  min.e <- sim.limits[3,2]
		  max.e <- sim.limits[3,3]
		}	else	if (rlprompt == "humerus"){
		  min.e <- sim.limits[4,2]
		  max.e <- sim.limits[4,3]
		}	else	if (rlprompt == "tibia"){
		  min.e <- sim.limits[1,2]
		  max.e <- sim.limits[1,3]
		}	else	if (rlprompt == "femur"){
		  min.e <- sim.limits[2,2]
		  max.e <- sim.limits[2,3]
		}
		
		# Store entered element type in the main data frame that the
		# function will return.
		dataset[a,1] <- rlprompt
		
		# Prompt user to enter metric value for the element that has
		# been entered.
		cat("Enter the minimum antero-posterior diaphyseal",
        "\ndepth of this element in mm (format xx.xx): ")
		metric.value <- readline()

		# Ensure that the entered value is within the simulated metric limits
		# for the element type. Also ensure that the entry is numeric. If (while)
		# these conditions are not satisfied, display an error message and prompt
		# the user the re-enter the metric value.
		while (is.na(suppressWarnings(as.numeric(as.character(metric.value)))) | 
		       suppressWarnings(as.numeric(as.character(metric.value))) <= min.e | 
		       suppressWarnings(as.numeric(as.character(metric.value))) >= max.e){
		  cat("ERROR: depth value must be numeric and fall between ",
		      round(min.e, digits=2), " and ", round(max.e, digits=2), 
          ".\nEnter the value for the depth of this element (format xx.xx): ", sep="")
		  metric.value <- readline()
		  }
		dataset[a,2] <- as.numeric(as.character(metric.value))
	  }
  }
  
  # Guide user through upload process.
  else if (entry.choice == "u"){
    # Prompt user to upload csv table.
    cat("\nEnsure that your csv table has only two columns. The first column contains",
        "\nelement types (valid types include 'radius', 'humerus', tibia', and 'femur')",
        "\nand the second column contains metric depth values, expressed in mm (e.g., 6.78,",
        "\n72.1, 110). The first row is read as a header.",
        "\n\nPress any key to select the csv file.")
    line <- readline()
    
    # Import table selected by user.Read this table into two different vectors,
    # one each for element type and metric values. Stoer them as character and
    # numeric vectors respectively.
    test.table <- data.frame(read.csv(file.choose(), header=TRUE))
    names.vec <- suppressWarnings(as.character(test.table[,1]))
    metric.vec <- suppressWarnings(as.numeric(as.character(test.table[,2])))
    
    # Ensure that element names are valid. If any name is not valid, replace
    # the existing string in the invalid position with "invalid".
    for (k in 1:length(names.vec)){
      if(names.vec[k] != "radius" && names.vec[k] != "humerus" &&
         names.vec[k] != "tibia" && names.vec[k] != "femur"){
        names.vec[k] <- "invalid"  
      }
    }
    
    # Ensure that table contains valid data. This accounts for uneven
    # data columns, NA values, or invalid element types. Given any of
    # these situations, display an error message and prompt the user
    # to re-enter a corrected csv table.
    while(length(names.vec) != length(metric.vec) |
          anyNA(names.vec) | anyNA(metric.vec) |
          any(names.vec == "invalid")){
      cat("\nERROR: Invalid element names or uneven column lengths.",
          "\nEnsure that csv data is accurate.",
          "\nPress any key to retry csv upload.")
      line <- readline()
      test.table <- data.frame(read.csv(file.choose(), header=TRUE))
      names.vec <- suppressWarnings(as.character(test.table[,1]))
      metric.vec <- suppressWarnings(as.numeric(as.character(test.table[,2])))
      
      # Recheck names validity for a re-entered csv table.
      for (k in 1:length(names.vec)){
        if(names.vec[k] != "radius" && names.vec[k] != "humerus" &&
           names.vec[k] != "tibia" && names.vec[k] != "femur"){
          names.vec[k] <- "invalid"  
        }
      }
    }
    
    # For the re-entered table: cycle through the metric values in 
    # the uploaded table to make sure that they are within the 
    # simulated metric limits for each element. Begin by first 
    # initializing the variable "limitscheck" at 0.
    limitscheck <- 0
    for(z in 1:length(names.vec)){
      # If else statements that assign the simulated metric limits for each
      # element based on the entered element.
      if (names.vec[z] == "radius"){
        min.e <- sim.limits[3,2]
        max.e <- sim.limits[3,3]
      }	else	if (names.vec[z] == "humerus"){
        min.e <- sim.limits[4,2]
        max.e <- sim.limits[4,3]
      }	else	if (names.vec[z] == "tibia"){
        min.e <- sim.limits[1,2]
        max.e <- sim.limits[1,3]
      }	else	if (names.vec[z] == "femur"){
        min.e <- sim.limits[2,2]
        max.e <- sim.limits[2,3]
      }
      # If a given value is outside its simulated limits, add
      # 1 to "limitscheck".  
      if(metric.vec[z] < min.e | metric.vec[z] > max.e){
        limitscheck <- limitscheck + 1
      }
    }
    
    # If out of range (limitscheck > 0), display error message and prompt
    # user to upload new table.
    while (limitscheck > 0){
      cat("\nERROR: Metric values fall outside of the range of modelled values.",
          "\n\nPress any key to upload a new csv file.")
      line <- readline()
      test.table <- read.csv(file.choose(), header=TRUE, sep=",")
      names.vec <- suppressWarnings(as.character(test.table[,1]))
      metric.vec <- suppressWarnings(as.numeric(as.character(test.table[,2])))
      
      # Within new table, ensure that element names are valid. If any element
      # type is not valid, assign it the string "invalid".
      for (k in 1:length(names.vec)){
        if(names.vec[k] != "radius" && names.vec[k] != "humerus" &&
           names.vec[k] != "tibia" && names.vec[k] != "femur"){
          names.vec[k] <- "invalid"  
        }
      }
      
      # Within the new table, ensure that table contains valid data including
      # no NA values, valid element types, and equal column lengths. If these
      # conditions are not satisified, prompt user to re-enter a corrected
      # csv table.
      while(length(names.vec) != length(metric.vec) |
            anyNA(names.vec) | anyNA(metric.vec) |
            any(names.vec == "invalid")){
        cat("\nERROR: Invalid element names or uneven column lengths. Ensure",
            "that csv data is accurate.\n\nPress any key to retry csv upload.")
        line <- readline()
        test.table <- read.csv(file.choose(), header=TRUE, sep=",")
        names.vec <- suppressWarnings(as.character(test.table[,1]))
        metric.vec <- suppressWarnings(as.numeric(as.character(test.table[,2])))
      }
      
      # For the re-entered table: cycle through the metric values in 
      # the uploaded table to make sure that they are within the 
      # simulated metric limits for each element. Begin by first 
      # initializing the variable "limitscheck" at 0.
      limitscheck <- 0
      for(z in 1:length(names.vec)){
        # If else statements that assign the simulated metric limits for each
        # element based on the entered element.
        if (names.vec[z] == "radius"){
          min.e <- sim.limits[3,2]
          max.e <- sim.limits[3,3]
        }	else	if (names.vec[z] == "humerus"){
          min.e <- sim.limits[4,2]
          max.e <- sim.limits[4,3]
        }	else	if (names.vec[z] == "tibia"){
          min.e <- sim.limits[1,2]
          max.e <- sim.limits[1,3]
        }	else	if (names.vec[z] == "femur"){
          min.e <- sim.limits[2,2]
          max.e <- sim.limits[2,3]
        }
        # If a given value is outside its simulated limits, add
        # 1 to "limitscheck".  
        if(metric.vec[z] < min.e | metric.vec[z] > max.e){
          limitscheck <- limitscheck + 1
          }
      }
    }
    
    # Save valid table as new data frame.
    dataset <- data.frame(names.vec, metric.vec, stringsAsFactors=FALSE)
    colnames(dataset) <- c("element.type","metric.value")
    
    }

	# Return the data frame of elements and their metric values.
	return(dataset)
}



# FUNCTION: Generates a vector of probabilities for every element
# entered using the entered.data() function. This function does not 
# return any variables, although it prints gestation ages ranges,
# date ranges, and graphical output for each element entry.
# Additionally, it presents the option for further analysis with
# the analysis.interval() and analysis.combined() functions.
main.function <- function(){

	# Initialize a vectors for a calendar that spans a full bison
	# reproductive cycle (from the first conception to the final
  # birth). This runs longer than a year (548 days), and therefore 
  # has repeated dates. This calendar begins on the
  # same day as the gestation onset calendars, June 1. Also, 
  # initialize vectors for a calendar that folds this reproductive 
  # cycle into one 365 day year with no duplicate dates. This
  # calendar begins on January 1.
  Month <- vector(mode="character", length=579)
  Month.adjusted <- vector(mode="character", length=365)
  Date <- vector(mode="numeric", length=579)
  DeathProb <- rep(0,579)
  Day <- seq(1:579)
  # Day.adjusted indexes the 579 day calendar to the 365 day
  # calendar.
  Day.adjusted <- rep(0, 579)
  
  Date.adjusted <- vector(mode="numeric", length=365)
  Day.adjs <- rep(1:365)
  DeathProb.adjusted <- rep(0,365)
  
  # Populate month and date vectors with their respective values
  # for the reproductive cycle.
  Month[1:30] <- "Jun"
  Date[1:30] <- rep(1:30)
  Day.adjusted[1:30] <- rep(152:181)
  
  Month[31:61] <- "Jul"
  Date[31:61] <- rep(1:31)
  Day.adjusted[31:61] <- rep(182:212)
  
  Month[62:92] <- "Aug"
  Date[62:92] <- rep(1:31)
  Day.adjusted[62:92] <- rep(213:243)
  
  Month[93:122] <- "Sep"
  Date[93:122] <- rep(1:30)
  Day.adjusted[93:122] <- rep(244:273)
  
  Month[123:153] <- "Oct"
  Date[123:153] <- rep(1:31)
  Day.adjusted[123:153] <- rep(274:304)
  
  Month[154:183] <- "Nov"
  Date[154:183] <- rep(1:30)
  Day.adjusted[154:183] <- rep(305:334)
  
  Month[184:214] <- "Dec"
  Date[184:214] <- rep(1:31)
  Day.adjusted[184:214] <- rep(335:365)
  
  Month[215:245] <- "Jan"
  Date[215:245] <- rep(1:31)
  Day.adjusted[215:245] <- rep(1:31)
  
  Month[246:273] <- "Feb"
  Date[246:273] <- rep(1:28)
  Day.adjusted[246:273] <- rep(32:59)
  
  Month[274:304] <- "Mar"
  Date[274:304] <- rep(1:31)
  Day.adjusted[274:304] <- rep(60:90)
  
  Month[305:334] <- "Apr"
  Date[305:334] <- rep(1:30)
  Day.adjusted[305:334] <- rep(91:120)
  
  Month[335:365] <- "May"
  Date[335:365] <- rep(1:31)
  Day.adjusted[335:365] <- rep(121:151)
  
  Month[366:395] <- "Jun"
  Date[366:395] <- rep(1:30)
  Day.adjusted[366:395] <- rep(152:181)
  
  Month[396:426] <- "Jul"
  Date[396:426] <- rep(1:31)
  Day.adjusted[396:426] <- rep(182:212)
  
  Month[427:457] <- "Aug"
  Date[427:457] <- rep(1:31)
  Day.adjusted[427:457] <- rep(213:243)
  
  Month[458:487] <- "Sep"
  Date[458:487] <- rep(1:30)
  Day.adjusted[458:487] <- rep(244:273)
  
  Month[488:518] <- "Oct"
  Date[488:518] <- rep(1:31)
  Day.adjusted[488:518] <- rep(274:304)
  
  Month[519:548] <- "Nov"
  Date[519:548] <- rep(1:30)
  Day.adjusted[519:548] <- rep(305:334)
  
  Month[549:579] <- "Dec"
  Date[549:579] <- rep(1:31)
  Day.adjusted[549:579] <- rep(335:365)
  
  # Combine all reproductive cycle calendar values into a data frame.
  calendar.template <- data.frame(Month, Date, Day, Day.adjusted, 
                                  DeathProb, stringsAsFactors=FALSE)
  
  # Populate month and date values for the 365 day year
  # using the values specified for the reproductive cycle.
  Month.adjusted[1:365] <- Month[215:579]
  Date.adjusted[1:365] <- Date[215:579]

	# Input all 365 day calendar values into a data frame.
	calendar.template.adjusted <- data.frame(Month.adjusted, Date.adjusted,
		Day.adjs, DeathProb.adjusted, stringsAsFactors=FALSE)

	# Call function to prompt user to select a gestation onset 
	# calendar vector. Assign this schedule to a new variable.
	selected.calendar <- select.model()

	# Run est.function to generate simulated length values.
	sim.metric <- est.function()
	
	# Create regression coefficients for diaphyseal length-depth relationships.
	cfs <- metric.function()
	
	# Create data frame to store the min and max simulated depth values for
	# each element.
	simulated.limits <- data.frame(element=c("tibia","femur","radius","humerus"),
	                               min=rep(0,4), max=rep(0,4), stringsAsFactors=FALSE)
	
  # Obtain min and max depth values based on the simulated lengths and quantile
	# regression coefficients (95% intervals).
	for(z in simulated.limits$element){
	  place <- which(simulated.limits[,1]==z)
	  a <- cfs[which(cfs[,1]==z & cfs[,2]=="97.5%"),3]
	  b <- cfs[which(cfs[,1]==z & cfs[,2]=="97.5%"),4]
	  simulated.limits[place,2] <- exp((log(sim.metric[1,(place*2-1)])-a)/b)
	  a <- cfs[which(cfs[,1]==z & cfs[,2]=="2.5%"),3]
	  b <- cfs[which(cfs[,1]==z & cfs[,2]=="2.5%"),4]
	  simulated.limits[place,3] <- exp((log(sim.metric[320,(place*2)])-a)/b)
	}
	
	# Prompt user to enter metric values and store them in a data frame.
	element.df <- entered.data(simulated.limits)

	# Create a matrix of zeros for the calendar year with rows
	# corresponding to the number of element entries specified by
	# the user.
	calendar.matrix <- matrix(0, 365, nrow(element.df))
	
	# Create matrix to store min and max gestation ages for each element.
	agerange <- matrix(0, nrow(element.df), 2)

	# For each entry...:
	for (x in 1:nrow(element.df)){
	  
		# ...find the element type and assign
		# its corresponding simulated values.
	  # Also retrieve the element specific
	  # regression coefficients to translate
	  # depth metrics into length metrics.
		if (element.df[x,1]=="radius"){
			min.func <- sim.metric[,6]
			max.func <- sim.metric[,5]
			coeffs <- cfs[cfs$models=="radius",]
			}
		else if (element.df[x,1]=="humerus"){
			min.func <- sim.metric[,8]
			max.func <- sim.metric[,7]
			coeffs <- cfs[cfs$models=="humerus",]
			}
		else if (element.df[x,1]=="tibia"){
			min.func <- sim.metric[,2]
			max.func <- sim.metric[,1]
			coeffs <- cfs[cfs$models=="tibia",]
			}
		else if (element.df[x,1]=="femur"){
			min.func <- sim.metric[,4]
			max.func <- sim.metric[,3]
			coeffs <- cfs[cfs$models=="femur",]
			}

	  # Create min and max values for the user-entered depths
	  # based on the measurement error and the regression
	  # estimates of length based on depth.
	  metric.min <- exp(coeffs[1,3] + coeffs[1,4]*log((element.df[x,2]-0.225)))
	  metric.max <- exp(coeffs[2,3] + coeffs[2,4]*log((element.df[x,2]+0.225)))
	  
	  # Find position of simulated value within the tolerances,
	  # and return the corresponding day value. Store these days
	  # in the matrix of age ranges.
	  agerange[x,1] <- min.day <- min(which(min.func >= metric.min))
	  agerange[x,2] <- max.day <- max(which(max.func <= metric.max))
    
		# Initialize data frame for the death date probability using the
		# calendar template specified earlier in the function.
		element.calendar <- calendar.template

		# For each potential gestation age y in min.day to max.day: reproduce
		# the conception probability distribution in an interval that is
		# y gestation days advanced from the conception dates. These, in
		# effect, these are the death date probabilities. Finally, the loop 
		# sums probabilities in each iteration. This is done for the full
		# reproductive cycle calendar.
		for (y in min.day:max.day){
			ymax <- y + length(selected.calendar) - 1
			element.calendar$DeathProb[y:ymax] <- element.calendar$DeathProb[y:ymax]+selected.calendar
			}

		# Assign the adjusted calendar template to a new adjusted calendar
		# data frame for the specific element.
		element.calendar.adjs <- calendar.template.adjusted

		# Assign the summed death date probabilities from the full reproductive cycle
		# calendar to the 365 day calendar.
		element.calendar.adjs$DeathProb.adjusted[152:365] <- element.calendar$DeathProb[1:214] + element.calendar$DeathProb[366:579]
		element.calendar.adjs$DeathProb.adjusted[1:151] <- element.calendar$DeathProb[215:365]

		# Standardize the summed probabilities to sum to 1.
		element.calendar.adjs$DeathProb.adjusted <- element.calendar.adjs$DeathProb.adjusted/sum(element.calendar.adjs$DeathProb.adjusted)

 ### Print the probability distributions: begin ###
		
		# Create empty vectors to store up to 365 probability masses. These
		# capture discrete intervals when the prob. dist. is discontinuous across
		# the calendar year. The vectors store the upper and lower bounds for
		# each interval.
		dist.lb <- rep(0, 365)
		dist.ub <- rep(0, 365)
		
		# Create numeric variables to track the number of date intervals.
		dist.lb.count <- 0
		dist.ub.count <- 0
		
		# For each day in the year, detect if the day represents the beginning
		# or ending of a date interval. This is accomplished by examining
		# the location of 0 probs that are adjacent to >0 probs in the 
		# modelled distribution.
		for (u in 1:364){
		  if (element.calendar.adjs$DeathProb.adjusted[u] == 0 & 
		      element.calendar.adjs$DeathProb.adjusted[u+1] > 0){
		    dist.lb.count <- dist.lb.count + 1
		    dist.lb[dist.lb.count] <- u+1
		    }
		  else if (dist.lb.count > 0 & 
		           element.calendar.adjs$DeathProb.adjusted[u+1] == 0 & 
		           element.calendar.adjs$DeathProb.adjusted[u] > 0){
		    dist.ub.count <- dist.ub.count + 1
		    dist.ub[dist.ub.count] <- u
		    }
		  else if (u == 1 & element.calendar.adjs$DeathProb.adjusted[u] > 0 & 
		           element.calendar.adjs$DeathProb.adjusted[365] == 0){
		    dist.lb.count <- dist.lb.count + 1
		    dist.lb[dist.lb.count] <- u
		    }
		  else if (u == 365 & element.calendar.adjs$DeathProb.adjusted[u] > 0 & 
		           element.calendar.adjs$DeathProb.adjusted[1] == 0){
		    dist.ub.count <- dist.ub.count + 1
		    dist.ub[dist.ub.count] <- u
		    }
		  }
		
		# If an interval overlaps Dec 31 - Jan 1, the preceding for loop will
		# not have found the position of upper bound for the final interval.
		# For this scenario, this if statement finds this final upper bound.
		if (max(which(element.calendar.adjs$DeathProb.adjusted > 0)) == 365 & 
		    min(which(element.calendar.adjs$DeathProb.adjusted > 0)) == 1){
		  dist.ub.count <- dist.ub.count+1
		  dist.ub[dist.ub.count] <- min(which(element.calendar.adjs$DeathProb.adjusted == 0)) - 1
		  }
		
    cat("\n\nEntry ",x," (",element.df[x,1],"; minimum antero-posterior diaphyseal depth: ",
        element.df[x,2]," mm).\n", "Estimated diaphyseal length: ", round(metric.min, digits=2), 
        " - ", round(metric.max, digits=2), " mm\n", "Estimated gestation age: ", min.day, 
        " - ", max.day, " days", sep="")
		
		# If the modelled distribution is continuously distributed (i.e., there is only one
		# upper and one lower bound), print the date range for the user.
		if (dist.lb.count == 1){
		  cat("\nThe SODE is distributed between ", 
		      element.calendar.adjs$Month.adjusted[dist.lb[1]], " ", 
		      element.calendar.adjs$Date.adjusted[dist.lb[1]], " and ", 
		      element.calendar.adjs$Month.adjusted[dist.ub[1]], " ", 
		      element.calendar.adjs$Date.adjusted[dist.ub[1]], ".", sep="")
		  }
		
		# If the modelled date is discontinuously distributed (i.e., there are multiple
		# upper and lower bounds), but none of intervals overlap Dec 31 - Jan 1,
		# display the date ranges and their associated probability masses to the
		# user.
		else if (max(which(element.calendar.adjs$DeathProb.adjusted > 0)) != 365 | 
		         min(which(element.calendar.adjs$DeathProb.adjusted > 0)) != 1){
		  cat("\nThe SODE is distributed across ", dist.lb.count, " intervals.", sep="")
		  
		  # This for loop prints the date ranges for each interval.
		  for (v in 1:dist.lb.count){
		    cat("\n", sum(element.calendar.adjs$DeathProb.adjusted[dist.lb[v]:dist.ub[v]]), 
		        " of the modelled date distribution falls between ", 
		        element.calendar.adjs$Month.adjusted[dist.lb[v]], " ", 
		        element.calendar.adjs$Date.adjusted[dist.lb[v]]," and ", 
		        element.calendar.adjs$Month.adjusted[dist.ub[v]], " ", 
		        element.calendar.adjs$Date.adjusted[dist.ub[v]], ".", sep="")
		    }
		  }
		
		# If the modelled date is discontinuously distributed (i.e., there are multiple
		# upper and lower bounds), and one interval overlaps Dec 31 - Jan 1,
		# display the date ranges and their associated probability masses to the
		# user.
		else {
		  cat("\nThe SODE is distributed across ", dist.lb.count, " intervals.", sep="")
		  forcount <- dist.lb.count-1
		  
		  # This for loop prints the date ranges for each interval. It skips the
		  # final interval since the vector positions in the last interval are
		  # in reverse ordinal order (ie, it would attempt to sum probabilities
		  # beginning at a high number and ending at a lower number).
		  for (v in 1:forcount){
		    cat("\n", sum(element.calendar.adjs$DeathProb.adjusted[dist.lb[v]:dist.ub[v]]), 
		        " of the modelled date distribution falls between ", 
		        element.calendar.adjs$Month.adjusted[dist.lb[v]], " ", 
		        element.calendar.adjs$Date.adjusted[dist.lb[v]]," and ", 
		        element.calendar.adjs$Month.adjusted[dist.ub[v]], " ", 
		        element.calendar.adjs$Date.adjusted[dist.ub[v]], ".", sep="")
		    }
		  
		  # These statements sum the probabilities in each portion of the final
		  # date interval and print the results in a statement similar to the one
		  # contained in the preceding for loop.
		  final.sum <- sum(element.calendar.adjs$DeathProb.adjusted[dist.lb[dist.lb.count]:365])+
		    sum(element.calendar.adjs$DeathProb.adjusted[1:dist.ub[dist.ub.count]])
		  cat("\n", final.sum, " of the modelled date distribution falls between ", 
		      element.calendar.adjs$Month.adjusted[dist.lb[dist.lb.count]], " ", 
		      element.calendar.adjs$Date.adjusted[dist.lb[dist.lb.count]], " and ", 
		      element.calendar.adjs$Month.adjusted[dist.ub[dist.lb.count]], " ", 
		      element.calendar.adjs$Date.adjusted[dist.ub[dist.lb.count]], ".", sep="")
		  }
    
  ### Print the probability distributions: end ###

		# Store death date distribution in a column of the calendar.matrix, which,
		# as a whole, will contain columns of death date probabilities for every
		# element entered by the user.
		calendar.matrix[,x] <- element.calendar.adjs$DeathProb.adjusted
		}
	
	# Add column of days (365) to the matrix containing columns of 
	# death date probabilities for each element. Melt into plottable
	# data frame, where id.vars marks which probabilites belong to 
	# each distribution.
	calendar.melted <- melt(data.frame(calendar.matrix, Day.adjs), id.vars="Day.adjs")
	
	# Change default ID variable to 'Entry' and remove 'X' prefix.
	colnames(calendar.melted)[2] <- "Entry"
  calendar.melted$Entry <- substring(calendar.melted$Entry, 2)
  if (any(calendar.melted$Entry == "alendar.matrix")){
    calendar.melted$Entry <- "1"
  }
	
  # Set Y-height variables for text and geometry in plots.
  YMaxG <- max(apply(calendar.matrix, 2, max))*1.15
  YMaxG2 <- max(apply(calendar.matrix, 2, max))*1.02
  YMaxT <- max(apply(calendar.matrix, 2, max))*1.09
  
  # Plot death date distributions with grey rectangles and text
  # indicating months of a 365 day year.
  death.plot <- ggplot()+
    annotate("rect", xmin = p.x1, xmax = p.x2, ymin = YMaxG2, 
             ymax = YMaxG, alpha = 0.25)+
    annotate("text", x=p.month.xpos, y=YMaxT, label=p.month.name, size=3)+
    geom_step(data=calendar.melted, aes(x=Day.adjs, y=value, color=Entry),
              stat="identity", direction="hv", alpha=0.9)+
    xlab("Day-of-year")+ylab("p(deposition)")+
    theme(axis.line.x = element_line(colour = "grey"),
          axis.line.y = element_line(colour = "grey"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill=NA),
          panel.background = element_blank())+
    scale_y_continuous(limits=c(0,YMaxG), expand=c(0.02,0))+
    scale_x_continuous(breaks=c(31, 90, 151, 212, 273, 334), expand=c(0,0))

	# Display the plot.
	plot(death.plot)

	# Ask user if they would like to proceed with more analyses.
	cat("\n\nWould you like to further analyze these data ('y' to proceed, any other key to quit)? ")
	analysis.proc <- readline()

	# present user to further analysis options that call functions specified
	# earlier in this script.
	while(analysis.proc == "y"){
	  # If the user only entered one element, only prompt them to select the 
	  # interval analysis (combined element analysis requires metrics from 
	  # multiple elements).
		if (ncol(calendar.matrix) == 1){
			cat("\n['i'] Find the probability that a death date occurred",
					"\n   within a calendar interval of dates.\n\n")
		  cat("[any other key] Quit, no further analyses.\n\n")
			}

	  # Only call these functions if the user entered multiple elements.
	  else {
	    cat("\n['c'] Obtain a single SODE from multiple elements. If multiple",
	        "\n  elements are from a single fetus, they should represent the same",
	        "\n  death date. This function constrains the gestation age range from",
	        "\n  multiple elements to obtain a narrow and more informative SODE.")
	    cat("\n['i'] Find the probability that one or more elements share a death date",
	        "\n  within a date interval or at any point during the year.")
	    cat("\n[any other key] Quit, no further analyses.\n\n")
	  }
	  analysis.prompt <- readline()
	  cat("\n")
	  
	  # Call analysis.interval() or analysis.combined() functions based on user input.
	  if (analysis.prompt == "i"){
	    analysis.interval(calendar.matrix, Month.adjusted, Day.adjs, Date.adjusted)
	  }
	  else if (analysis.prompt == "c" & ncol(calendar.matrix) > 1){
	    # Store results of combined analysis as a new variable.
	    combined.dist <- analysis.combined(calendar.matrix, Month.adjusted, 
	                                       Date.adjusted, agerange, selected.calendar)
	    combined.row <- data.frame(combined.dist[1], NA, stringsAsFactors=FALSE)
	    # Assign
	    names(combined.row) <- names(element.df)
	    element.df <- rbind(element.df, combined.row)
	    cat("\nCombined distribution = Entry ", nrow(element.df),  ".\n", sep="")
	    calendar.matrix <- cbind(calendar.matrix, unlist(combined.dist[2]))
	  }
		# Quit if the user does not intend to do further analyses.
		else {
		  cat("\nGoodbye!\n\n")
		  return(NA)
		  }
		cat("\nWould you like to further analyze these data",
        "('y' to proceed, any other key to quit)? ")
		analysis.proc <- readline()
	  }
	cat("\nGoodbye!\n\n")
	return(NA)
}


# FUNCTION: This function simulates tibia, femur, radius,  and humerus
# length values over a 320 day gestation period for bison. Additionally,
# it gives users the option to plot those simulated growth values. The
# function takes no arguments. It returns the data frame of simulated
# values.
est.function <- function(){
  
  # Simulate fetal weights for historic (recieved supplemental feed)
  # and modern (no supplemental feed) bison over a 320 day gestation
  # period. The vast majority of real gestation periods are shorter
  # than 320 days, but this will allow unusually long pregnancies to
  # be captured in the growth model. In other words, most calves will
  # be born earlier and with smaller biometric values than those
  # indicated in the far right portion of the growth models. Weight 
  # formulas from Gogan et al. (2005:1722). Since Gogan et al.'s 
  # formulas estimate cube root weight, these have been cubed. Weights 
  # are in grams.
  #
  # Data reference: Gogan PJP, Podruzny KM, Olexa EM, Pac HI, Frey
  #                 KL, 2005. Yellowstone bison fetal development 
  #                 and phenology of parturition. J. Wildl. Manag. 
  #                 69, 1716-1730.
  days <- rep(1:320)
  wt.modernfemales <- (57.26/(1+(days/275.71)^-2.05))^3
  wt.modernmales <- (66.79/(1+(days/300.66)^-2.04))^3
  wt.historicfemales <- (67.54/(1+(days/302.53)^-2.04))^3
  wt.historicmales <- (80.82/(1+(days/334.26)^-2.03))^3
  
  # Simulate fetal crown-rump lengths (CRL) based on the simulated
  # weights. These equations are based on Gogan et al. (2005:1724),
  # who found that weight was equal to CRL^2.95. This relationship
  # was scaled differently between herds, but the scale was not
  # stated in the publication. To estimate the scale, CRL values
  # were estimated for each function when weight = 35000.
  # Therefore: 35000 = (CRLh^2.95)/x, where CRLh is the estimated CRL
  # for a given herd at 35000 gr, and x is the scaling factor. Herd 
  # A is based on historic weights, the rest are based on modern 
  # weights. Calculate the sex averaged CRL for each herd.
  #
  # Data reference: Gogan PJP, Podruzny KM, Olexa EM, Pac HI, Frey
  #                 KL, 2005. Yellowstone bison fetal development 
  #                 and phenology of parturition. J. Wildl. Manag. 
  #                 69, 1716-1730.
  crl.males.a <- (11893*wt.historicmales)^(1/2.95)
  crl.females.a <- (11893*wt.historicfemales)^(1/2.95)
  crl.a <- (crl.males.a + crl.females.a)/2
  crl.males.b <- (15193*wt.modernmales)^(1/2.95)
  crl.females.b <- (15193*wt.modernfemales)^(1/2.95)
  crl.b <- (crl.males.b + crl.females.b)/2
  crl.males.ce <- (13859*wt.modernmales)^(1/2.95)
  crl.females.ce <- (13859*wt.modernfemales)^(1/2.95)
  crl.ce <- (crl.males.ce + crl.females.ce)/2
  crl.males.d <- (15416*wt.modernmales)^(1/2.95)
  crl.females.d <- (15416*wt.modernfemales)^(1/2.95)
  crl.d <- (crl.males.d + crl.females.d)/2
  
  # Estimate variation around the mean cattle growth values using
  # data from Richardson et al. (1990). Richardson et al. present
  # 95% tolerance limits for metrics at a series of points during
  # gestation at 6 gestation ages. This block of script uses these
  # values to convert tolerance length values into ratios of mean
  # length values.
  #
  # The 95% (1.96 stdv) limits are converted to 99.73% (3 stdv) limits
  # for more inclusive predictive value. The 95% limits are denoted
  # tol.t/r.min/max95, and the 99.73% limits lack the 95 suffix.
  #
  # Data reference: Richardson C, Jones PC, Barnard V, Hebert CN,
  #                 Terlecki S, Wijeratne WVS, 1990. Estimation of
  #                 the developmental age of the bovine fetus and
  #                 newborn calf. Vet. Rec. 126, 279-284.
  
  tol.days <- c(100, 140, 160, 180, 220, 260) # gestation ages
  tol.n <- c(6, 7, 7, 7, 9, 11) # samples sizes at each age
  tol.crl <- c(192, 334, 411, 495, 670, 845) # CRL
  
  tol.t.max95 <- c(20.8, 43.3, 56.8, 79.2, 123.8, 173.4) # tib len 97.5% tol lim
  tol.t.avg <- c(18.5, 41.0, 54.3, 73.2, 116.2, 164.2) # tib avg
  tol.t.min95 <- c(16.2, 38.6, 51.9, 67.4, 108.8, 155.3) # tib len 2.5% tol lim
  tol.t.max <- (3*(tol.t.max95-tol.t.avg)/1.96)+tol.t.avg  # tib len 99.85% tol lim
  tol.t.min <- tol.t.avg-(3*(tol.t.avg-tol.t.min95)/1.96)  # tib len 0.15% tol lim
  
  tol.r.max95 <- c(18.3, 36.6, 47.2, 65.3, 100.3, 135.3) # rad len 97.5% tol lim
  tol.r.avg <- c(16.3, 34.6, 45.2, 59.2, 93.1, 126.7) # rad avg
  tol.r.min95 <- c(14.4, 32.7, 43.2, 53.5, 86.0, 118.4) # rad len 2.5% tol lim
  tol.r.max <- (3*(tol.r.max95-tol.r.avg)/1.96)+tol.r.avg  # rad len 99.85% tol lim
  tol.r.min <- tol.r.avg-(3*(tol.r.avg-tol.r.min95)/1.96)  # rad len 0.15% tol lim
  
  cow.ratios.df <- data.frame(tol.days, tol.t.max, tol.t.min,
                              tol.r.max, tol.r.min, tol.crl, tol.n)
  
  # Model minimum and maximum lengths for tibiae and radii
  # as logistic growth functions. Weight data points by the
  # sample sizes reported in Richardson et al. (1990). Fit a
  # similar model to fetal cattle CRL.
  #
  # Data reference: Richardson C, Jones PC, Barnard V, Hebert CN,
  #                 Terlecki S, Wijeratne WVS, 1990. Estimation of
  #                 the developmental age of the bovine fetus and
  #                 newborn calf. Vet. Rec. 126, 279-284.
  tmax.nls <- nls(tol.t.max ~ p1/(1+p2*exp(1)^((-1)*p3*tol.days)), 
                  data=cow.ratios.df, weights=tol.n, 
                  start=list(p1=500, p2=500, p3=0.02))
  tmin.nls <- nls(tol.t.min ~ p1/(1+p2*exp(1)^((-1)*p3*tol.days)), 
                  data=cow.ratios.df, weights=tol.n, 
                  start=list(p1=500, p2=500, p3=0.02))
  rmax.nls <- nls(tol.r.max ~ p1/(1+p2*exp(1)^((-1)*p3*tol.days)), 
                  data=cow.ratios.df, weights=tol.n, 
                  start=list(p1=500, p2=500, p3=0.02))
  rmin.nls <- nls(tol.r.min ~ p1/(1+p2*exp(1)^((-1)*p3*tol.days)), 
                  data=cow.ratios.df, weights=tol.n, 
                  start=list(p1=500, p2=500, p3=0.02))
  crl.nls <- nls(tol.crl ~ p1/(1+p2*exp(1)^((-1)*p3*tol.days)), 
                 data=cow.ratios.df, weights=tol.n, 
                 start=list(p1=900, p2=20, p3=0.018))
  
  # Simulate values for the logistic models, and also estimate
  # average length values from the simulated max and min values.
  rad.max <- predict(rmax.nls, list(tol.days=sort(days)))
  rad.min <- predict(rmin.nls, list(tol.days=sort(days)))
  rad.avg <- (rad.max+rad.min)/2
  tib.max <- predict(tmax.nls, list(tol.days=sort(days)))
  tib.min <- predict(tmin.nls, list(tol.days=sort(days)))
  tib.avg <- (tib.max+tib.min)/2
  crl.sim <- predict(crl.nls, list(tol.days=sort(days)))
  
  # Create vectors that express the max and min tolerance
  # values as proportions of the averages. 
  rad.pmax <- rad.max/rad.avg
  rad.pmin <- rad.min/rad.avg
  tib.pmax <- tib.max/tib.avg
  tib.pmin <- tib.min/tib.avg
  
  # Create vectors that expresses average values as proportions of CRL.
  rad.crl <- rad.avg/crl.sim
  tib.crl <- tib.avg/crl.sim
  
  # Scale the cow gestation period with an average 280 day length (Norman 
  # et al. 2009:2261) to the gestation period for historic and modern
  # bison, which have average lengths of 265 and 272 days (Gogan et al.
  # 2005). Combine these scaled growth periods into a data frame with
  # the simulated proportional element length growth values.
  #
  # Data references: Gogan PJP, Podruzny KM, Olexa EM, Pac HI, Frey
  #                 KL, 2005. Yellowstone bison fetal development 
  #                 and phenology of parturition. J. Wildl. Manag. 
  #                 69, 1716-1730.
  #                 Norman HD, Wright JR, Kuhn MT, Hubbard SM, Cole
  #                 JB, VanRaden PM, 2009. Genetic and environmental
  #                 factors that affect gestation length in dairy
  #                 cattle. J. Dairy Sci. 92, 2259-2269.
  hist.days <- (days/280)*265 # convert 320 day vector of cow gestation days
                              # into a 320 position vector where days have
                              # been rescaled to a 265 day gestation period
                              # for bison in the historic period.
  mod.days <- (days/280)*272  # convert 320 day vector of cow gestation days
                              # into a 320 position vector where days have
                              # been rescaled to a 272 day gestation period
                              # for modern bison.
  bison.scaling.df <- data.frame(hist.days, mod.days, rad.pmax, rad.pmin,
                                 rad.crl, tib.pmax, tib.pmin, tib.crl)
  
  # Use a polynomial regression to describe the relationship beween the scaled
  # days vectors for bison and max/min element lengths. This allows for the
  # simulated bison values to be calculated for each day in a 320 day gestation
  # period, rather than across the days in the 320 position vector.
  b.hist.rad.max <- lm(rad.pmax~hist.days+I(hist.days^2)+I(hist.days^3)+I(hist.days^4)+
                         I(hist.days^5),data=bison.scaling.df) # adjs R-sq > 0.999
  b.hist.rad.min <- lm(rad.pmin~hist.days+I(hist.days^2)+I(hist.days^3)+I(hist.days^4)+
                         I(hist.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.hist.rad.crl <- lm(rad.crl~hist.days+I(hist.days^2)+I(hist.days^3)+I(hist.days^4)+
                         I(hist.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.hist.tib.max <- lm(tib.pmax~hist.days+I(hist.days^2)+I(hist.days^3)+I(hist.days^4)+
                         I(hist.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.hist.tib.min <- lm(tib.pmin~hist.days+I(hist.days^2)+I(hist.days^3)+I(hist.days^4)+
                         I(hist.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.hist.tib.crl <- lm(tib.crl~hist.days+I(hist.days^2)+I(hist.days^3)+I(hist.days^4)+
                         I(hist.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.mod.rad.max <- lm(rad.pmax~mod.days+I(mod.days^2)+I(mod.days^3)+I(mod.days^4)+
                        I(mod.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.mod.rad.min <- lm(rad.pmin~mod.days+I(mod.days^2)+I(mod.days^3)+I(mod.days^4)+
                        I(mod.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.mod.rad.crl <- lm(rad.crl~mod.days+I(mod.days^2)+I(mod.days^3)+I(mod.days^4)+
                        I(mod.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.mod.tib.max <- lm(tib.pmax~mod.days+I(mod.days^2)+I(mod.days^3)+I(mod.days^4)+
                        I(mod.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.mod.tib.min <- lm(tib.pmin~mod.days+I(mod.days^2)+I(mod.days^3)+I(mod.days^4)+
                        I(mod.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  b.mod.tib.crl <- lm(tib.crl~mod.days+I(mod.days^2)+I(mod.days^3)+I(mod.days^4)+
                        I(mod.days^5),data=bison.scaling.df) # adjs R-sq  > 0.999
  
  # Now that the gestation lengths are scaled for bison, simulate max and min tibia
  # and radius lengths for each herd x over a 320 day gestation period using the 
  # cow metrics:
  # 'max/min element length' = simulated CRL of herd x' * 'simulated element-length/
  # CRL scaling values adjusted for bison gestation length' * 'max/min scaling values 
  # for element length 99.73% tolerances that have been adjusted for bison gest. length'
  bh.a.rmax <- crl.a*predict(b.hist.rad.crl, list(hist.days=sort(days)))*
    predict(b.hist.rad.max, list(hist.days=sort(days)))
  bh.a.rmin <- crl.a*predict(b.hist.rad.crl, list(hist.days=sort(days)))*
    predict(b.hist.rad.min, list(hist.days=sort(days)))
  bh.a.tmax <- crl.a*predict(b.hist.tib.crl, list(hist.days=sort(days)))*
    predict(b.hist.tib.max, list(hist.days=sort(days)))
  bh.a.tmin <- crl.a*predict(b.hist.tib.crl, list(hist.days=sort(days)))*
    predict(b.hist.tib.min, list(hist.days=sort(days)))
  bh.b.rmax <- crl.b*predict(b.mod.rad.crl, list(mod.days=sort(days)))*
    predict(b.mod.rad.max, list(mod.days=sort(days)))
  bh.b.rmin <- crl.b*predict(b.mod.rad.crl, list(mod.days=sort(days)))*
    predict(b.mod.rad.min, list(mod.days=sort(days)))
  bh.b.tmax <- crl.b*predict(b.mod.tib.crl, list(mod.days=sort(days)))*
    predict(b.mod.tib.max, list(mod.days=sort(days)))
  bh.b.tmin <- crl.b*predict(b.mod.tib.crl, list(mod.days=sort(days)))*
    predict(b.mod.tib.min, list(mod.days=sort(days)))
  bh.ce.rmax <- crl.ce*predict(b.mod.rad.crl, list(mod.days=sort(days)))*
    predict(b.mod.rad.max, list(mod.days=sort(days)))
  bh.ce.rmin <- crl.ce*predict(b.mod.rad.crl, list(mod.days=sort(days)))*
    predict(b.mod.rad.min, list(mod.days=sort(days)))
  bh.ce.tmax <- crl.ce*predict(b.mod.tib.crl, list(mod.days=sort(days)))*
    predict(b.mod.tib.max, list(mod.days=sort(days)))
  bh.ce.tmin <- crl.ce*predict(b.mod.tib.crl, list(mod.days=sort(days)))*
    predict(b.mod.tib.min, list(mod.days=sort(days)))
  bh.d.rmax <- crl.d*predict(b.mod.rad.crl, list(mod.days=sort(days)))*
    predict(b.mod.rad.max, list(mod.days=sort(days)))
  bh.d.rmin <- crl.d*predict(b.mod.rad.crl, list(mod.days=sort(days)))*
    predict(b.mod.rad.min, list(mod.days=sort(days)))
  bh.d.tmax <- crl.d*predict(b.mod.tib.crl, list(mod.days=sort(days)))*
    predict(b.mod.tib.max, list(mod.days=sort(days)))
  bh.d.tmin <- crl.d*predict(b.mod.tib.crl, list(mod.days=sort(days)))*
    predict(b.mod.tib.min, list(mod.days=sort(days)))
  
  # Combine simulated max and min length values for each herd into two
  # data frames, one for the tibia and one for the radius.
  tibia.df <- data.frame(bh.a.tmax, bh.a.tmin, bh.b.tmax, bh.b.tmin, 
                         bh.ce.tmax, bh.ce.tmin, bh.d.tmax, bh.d.tmin)
  radius.df <- data.frame(bh.a.rmax, bh.a.rmin, bh.b.rmax, bh.b.rmin, 
                          bh.ce.rmax, bh.ce.rmin, bh.d.rmax, bh.d.rmin)
  
  # Find min and max simulated tibia and radius lengths between herds for each 
  # date, and add them a data frame. In the data frame, include empty columns
  # for femora, and humeri lengths.
  min.max.df <- data.frame(apply(tibia.df, 1, min), apply(tibia.df, 1, max), 
                           rep(0, 320), rep(0, 320), apply(radius.df, 1, min), 
                           apply(radius.df, 1, max), rep(0, 320), rep(0, 320))
  colnames(min.max.df) <- c("Tib.Min", "Tib.Max", "Fem.Min", "Fem.Max",
                            "Rad.Min", "Rad.Max", "Hum.Min", "Hum.Max")
  
  # The next few blocks of code estimate the relationship between radius/tibia
  # lengths and femur/humerus lengths.
  #
  # Humerus lengths (hl) radius lengths (rl), femur lengths (fl), and tibia
  # lengths (tl) for the UWAR specimens. For each element vector, the first 21 
  # positions are for left elements and the last 21 positions are for right
  # elements. Elements where length was not measurable are entered here as NA.
  # The final two entries in each vector are for left and right elements from
  # the YNP fetus.
  hl <- c(114.67, 123.08, 88.78, NA, 120.65, NA, 118.93, 88.70, 128.66, 132.79,
          119.47, 110.28, 115.51, NA, 132.97, 151.00, 130.61, 119.10, 130.15,
          115.48, 144.63, 113.85, 123.05, 88.56, 133.90, 120.80, NA, 119.96,
          88.92, 127.13, 134.29, 120.11, 111.11, 115.03, 133.67, 132.33, 148.10,
          130.96, 119.38, 131.19, 114.54, 144.98, NA, NA)
  rl <- c(113.37, 119.33, 89.54, NA, 116.53, NA, 118.40, 90.30, 123.19, 134.10,
          118.42, 107.75, 114.40, NA, 132.23, 141.57, 122.77, 114.21, 130.25, 
          112.02, 139.61, 113.36, 118.79, 89.79, 131.07, 117.55, NA, 118.11,
          90.13, 122.53, 134.37, 117.65, 107.94, 113.24, 132.49, 132.53, 140.15,
          122.17, 115.69, 130.17, 113.49, 139.11, NA, NA)
  fl <- c(128.57, 140.29, 103.99, NA, 137.04, NA, 136.77, 102.96, 146.74, 155.00,
          136.64, 125.30, 130.05, 153.00, 154.00, 159.62, 147.72, 134.77, 153.00,
          132.11, 158.00, 127.94, 140.63, 103.07, NA, 138.00, NA, 137.63, 103.66,
          144.58, 155.00, 136.21, 124.06, 130.01, 155.0, 155.0, 159.35, 147.03,
          132.68, 152.00, 133.76, 159.00, NA, 91.19)
  tl <- c(142.13, 151.00, 116.50, 169.00, 151.00, NA, 156.00, 116.95, 165.00,
          157.00, 145.41, 139.01, 146.78, 169.00, 167.00, 178.00, 163.00, 147.88,
          172.00, 147.25, 171.00, 141.26, 153.00, 115.34, 167.00, 149.34, NA,
          153.00, 116.05, 165.00, 163.00, 144.20, 138.77, 147.43, NA, 165.00, 
          180.00, 161.00, 149.33, 170.00, 147.37, 170.00, 102.61, 106.04)
  
  # Store lengths in a data frame.
  obs.lengths <- data.frame(hl, rl, fl, tl)
  
  # Use simple linear models to estimate predictive regression formulas
  # based on the elements that already have modeled growth curves (radius
  # and tibia). Set intercept to 0 to avoid models that estimate negative
  # element lengths. Use radius lengths to predict humerus values, and 
  # use the tibia to predict femur values.
  hlXrl <- lm(hl ~ 0 + rl, data=obs.lengths, na.action=na.omit) # adjs R-sq = 0.9995
  flXtl <- lm(fl ~ 0 + tl, data=obs.lengths, na.action=na.omit) # adjs R-sq = 0.9993
  
  # Simulate min and max femur and humerus lengths for the 320-day
  # gestation period.
  min.max.df$Fem.Min <- predict(flXtl, list(tl=min.max.df$Tib.Min))
  min.max.df$Fem.Max <- predict(flXtl, list(tl=min.max.df$Tib.Max))
  min.max.df$Hum.Min <- predict(hlXrl, list(rl=min.max.df$Rad.Min))
  min.max.df$Hum.Max <- predict(hlXrl, list(rl=min.max.df$Rad.Max))
  min.max.df$days <- rep(1:320)
  
  # Prompt user to adjust growth for antqiuus body size.
  cat("Would you like to adjust the growth curves to account for",
      "\nthe larger body mass of B. b. antiquus in comparison with",
      "\nmodern bison? Enter 'y' for this adjustment or any other",
      "\nkey to proceed using the modern growth curves.\n")
  
  ratioadjustment <- readline()
  
  # If user opts to adjust for antiquus body size, adjust upper
  # limits based on lengthratios.csv data. Store old upper limits
  # in new variables.
  if(ratioadjustment=="y"){
    # Check to see if lengthratios.csv already exists in the working
    # directory. If it does not, call function ratiosboot() to create it.
    if(isTRUE(file.exists("lengthratios.csv"))){
      adjustments <- read.csv("lengthratios.csv", header=TRUE)}
      else {
        bootratios()
        adjustments <- read.csv("lengthratios.csv", header=TRUE)}
    old.fem.max <- min.max.df$Fem.Max
    old.hum.max <- min.max.df$Hum.Max
    old.tib.max <- min.max.df$Tib.Max
    old.rad.max <- min.max.df$Rad.Max
    fmaxmax <- min.max.df$Fem.Max*adjustments$ub[3]
    fmaxmin <- min.max.df$Fem.Max*adjustments$lb[3]
    hmaxmax <- min.max.df$Hum.Max*adjustments$ub[1]
    hmaxmin <- min.max.df$Hum.Max*adjustments$lb[1]
    tmaxmax <- min.max.df$Tib.Max*adjustments$ub[4]
    tmaxmin <- min.max.df$Tib.Max*adjustments$lb[4]
    rmaxmax <- min.max.df$Rad.Max*adjustments$ub[2]
    rmaxmin <- min.max.df$Rad.Max*adjustments$lb[2]
    min.max.df$Fem.Max <- min.max.df$Fem.Max*adjustments$mean[3]
    min.max.df$Hum.Max <- min.max.df$Hum.Max*adjustments$mean[1]
    min.max.df$Tib.Max <- min.max.df$Tib.Max*adjustments$mean[4]
    min.max.df$Rad.Max <- min.max.df$Rad.Max*adjustments$mean[2]
    plotinfo <- data.frame(old.fem.max, old.hum.max, old.tib.max,
                      old.rad.max, fmaxmax, fmaxmin, hmaxmax,
                      hmaxmin, tmaxmax, tmaxmin, rmaxmax, rmaxmin,
                      days=rep(1:320))
    cat("\nUpper growth limits adjusted for antiquus.\n\n")
  }
  
  # Ask user whether they would like to plot the simulated values.
  cat("Would you like to plot the growth curves? Enter 'y' to view plots",
      "\nor any other key to proceed without viewing the plots.\n")
  growth.plotting <- readline()
  
  # Plot values if the user enters 'y'.
  if (growth.plotting == "y"){
    
    if (ratioadjustment == "y"){
      
      # Plot simulated growth values for antiquus.
      tibia.growth <- ggplot() +
        geom_ribbon(data=plotinfo, aes(x=days, ymin=tmaxmin, 
                  ymax=tmaxmax), alpha=0.2)+
        geom_line(data=plotinfo, aes(x=days, y=old.tib.max),
                  size=0.5, linetype="dotted")+
        geom_point(data=min.max.df, aes(x=days, y=Tib.Min), 
                   size=0.4, shape=20)+
        geom_point(data=min.max.df, aes(x=days, y=Tib.Max), 
                   size=0.4, shape=20)+
        annotate("text", x=10, y=max(0.9*plotinfo$tmaxmax), 
                 label="tibia", hjust=0, alpha=0.8)+
        xlab("Gestation age (days)") + ylab("Diaphysis length (mm)")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              panel.background = element_blank())
      
      femur.growth <- ggplot() +
        geom_ribbon(data=plotinfo, aes(x=days, ymin=fmaxmin, 
                      ymax=fmaxmax), alpha=0.2)+
        geom_line(data=plotinfo, aes(x=days, y=old.fem.max),
                   size=0.5, linetype="dotted")+
        geom_point(data=min.max.df, aes(x=days, y=Fem.Min), 
                   size=0.4, shape=20)+
        geom_point(data=min.max.df, aes(x=days, y=Fem.Max), 
                   size=0.4, shape=20)+
        annotate("text", x=10, y=(0.9*max(plotinfo$fmaxmax)), 
                  label="femur", hjust=0, alpha=0.8)+
        xlab("Gestation age (days)") + ylab("Diaphysis length (mm)")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              panel.background = element_blank())
      
      radius.growth <- ggplot() +
        geom_ribbon(data=plotinfo, aes(x=days, ymin=rmaxmin, 
                        ymax=rmaxmax), alpha=0.2)+
        geom_line(data=plotinfo, aes(x=days, y=old.rad.max),
                   size=0.5, linetype="dotted")+
        geom_point(data=min.max.df, aes(x=days, y=Rad.Min), 
                   size=0.4, shape=20)+
        geom_point(data=min.max.df, aes(x=days, y=Rad.Max), 
                   size=0.4, shape=20)+
        annotate("text", x=10, y=(0.9*max(plotinfo$rmaxmax)), 
                 label="radius", hjust=0, alpha=0.8)+
        xlab("Gestation age (days)") + ylab("Diaphysis length (mm)")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              panel.background = element_blank())
      
      humerus.growth <- ggplot() +
        geom_ribbon(data=plotinfo, aes(x=days, ymin=hmaxmin, 
                        ymax=hmaxmax), alpha=0.2)+
        geom_line(data=plotinfo, aes(x=days, y=old.hum.max),
                  size=0.5, linetype="dotted")+
        geom_point(data=min.max.df, aes(x=days, y=Hum.Min), 
                   size=0.4, shape=20)+
        geom_point(data=min.max.df, aes(x=days, y=Hum.Max), 
                   size=0.4, shape=20)+
        annotate("text", x=10, y=(0.9*max(plotinfo$hmaxmax)), 
                 label="humerus", hjust=0, alpha=0.8)+
        xlab("Gestation age (days)") + ylab("Diaphysis length (mm)")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              panel.background = element_blank())
      cat("Grey bands indicate 95% confidence intervals for the upper",
          "\nlimits of growth. For comparison, the upper limits of",
          "\nmodelled growth in modern bison are symbolized by light",
          "\ngrey curves.\n\n")
      
    } else {
      
      # Plot simulated growth values for modern bison.
      tibia.growth <- ggplot() +
        geom_point(data=min.max.df, aes(x=days, y=Tib.Min), 
                   size=0.4, shape=20)+
        geom_point(data=min.max.df, aes(x=days, y=Tib.Max), 
                   size=0.4, shape=20)+
        annotate("text", x=10, y=(0.9*max(min.max.df$Tib.Max)), 
                 label="tibia", hjust=0, alpha=0.8)+
        xlab("Gestation age (days)") + ylab("Diaphysis length (mm)")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              panel.background = element_blank())
      
      femur.growth <- ggplot() +
        geom_point(data=min.max.df, aes(x=days, y=Fem.Min), 
                   size=0.4, shape=20)+
        geom_point(data=min.max.df, aes(x=days, y=Fem.Max), 
                   size=0.4, shape=20)+
        annotate("text", x=10, y=(0.9*max(min.max.df$Fem.Max)), 
                 label="femur", hjust=0, alpha=0.8)+
        xlab("Gestation age (days)") + ylab("Diaphysis length (mm)")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              panel.background = element_blank())
      
      radius.growth <- ggplot() +
        geom_point(data=min.max.df, aes(x=days, y=Rad.Min), 
                   size=0.4, shape=20)+
        geom_point(data=min.max.df, aes(x=days, y=Rad.Max), 
                   size=0.4, shape=20)+
        annotate("text", x=10, y=(0.9*max(min.max.df$Rad.Max)), 
                 label="radius", hjust=0, alpha=0.8)+
        xlab("Gestation age (days)") + ylab("Diaphysis length (mm)")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              panel.background = element_blank())
      
      humerus.growth <- ggplot() +
        geom_point(data=min.max.df, aes(x=days, y=Hum.Min), 
                   size=0.4, shape=20)+
        geom_point(data=min.max.df, aes(x=days, y=Hum.Max), 
                   size=0.4, shape=20)+
        annotate("text", x=10, y=(0.9*max(min.max.df$Hum.Max)), 
                 label="humerus", hjust=0, alpha=0.8)+
        xlab("Gestation age (days)") + ylab("Diaphysis length (mm)")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              panel.background = element_blank())
      }
    
    grid.arrange(humerus.growth, radius.growth, femur.growth,
                 tibia.growth, ncol=2)
  }
  
  # Return data frame of simulated element lengths.
  return(min.max.df)
}


# FUNCTION: This function estimates diaphysis lengths from 
# minimum antero-posterior diaphysis depths. It requires
# no arguments. The function returns a data frame of regression
# coefficients.
metric.function <- function(){
  # Data frame of fetal bison metrics for diaphysis length
  # and minimum antero-posterior depth. Create a subset of this
  # data that excludes 8232B, a bison fetus with outlying metric
  # values.
  master1 <- read.csv("metrics.csv", header=TRUE, stringsAsFactors=FALSE)
  master1$llength <- log(master1$length)
  master1$ldepth <- log(master1$depth)
  
  # Parition metric data into element specific data frames.
  humerus <- master1[master1$element=="humerus",]
  radius <- master1[master1$element=="radius",]
  tibia <- master1[master1$element=="tibia",]
  femur <- master1[master1$element=="femur",]
  
  # Create a data frame to store parameter values for length-depth
  # relationships.
  models <- c("humerus", "humerus", "radius", "radius",
              "tibia", "tibia", "femur", "femur")
  taus <- rep(c("2.5%","97.5%"),4)
  modelspar <- data.frame(models=models, tau=taus, a=rep(0,8), 
                          b=rep(0,8), stringsAsFactors=FALSE)
  
  # Define generic function for the relationship between diaphysis
  # depth and length.
  func <- llength ~ a + b*ldepth
  
  # Paramaterize function for each element. This function is used to
  # estimate 2.5%, 50%, and 97.5% quantile relationships.
  sl <- list(a=0, b=1)
  hdl <- nlrq(func, data=humerus, tau=0.025,start=sl)
  hdm <- nlrq(func, data=humerus, tau=0.5,start=sl)
  hdu <- nlrq(func, data=humerus, tau=0.975,start=sl)
  rdl <- nlrq(func, data=radius, tau=0.025,start=sl)
  rdm <- nlrq(func, data=radius, tau=0.5,start=sl)
  rdu <- nlrq(func, data=radius, tau=0.975,start=sl)
  tdl <- nlrq(func, data=tibia, tau=0.025,start=sl)
  tdm <- nlrq(func, data=tibia, tau=0.5,start=sl)
  tdu <- nlrq(func, data=tibia, tau=0.975,start=sl)
  fdl <- nlrq(func, data=femur, tau=0.025,start=sl)
  fdm <- nlrq(func, data=femur, tau=0.5,start=sl)
  fdu <- nlrq(func, data=femur, tau=0.975,start=sl)
  
  # Store model parameters in modelspar data frame.
  modelspar[1,3:4] <- coef(hdl)
  modelspar[2,3:4] <- coef(hdu)
  modelspar[3,3:4] <- coef(rdl)
  modelspar[4,3:4] <- coef(rdu)
  modelspar[5,3:4] <- coef(tdl)
  modelspar[6,3:4] <- coef(tdu)
  modelspar[7,3:4] <- coef(fdl)
  modelspar[8,3:4] <- coef(fdu)
  
  # Present user with the option to plot the length-depth relationships.
  cat("Would you like to plot the relationships between diaphysis length",
      "\nand minimum antero-posterior depth? Enter 'y' to view plots or any",
      "\nother key to proceed without viewing the plots.\n")
  plotchoice <- readline()
  
  # if the user opts to plot the length-depth relationships...
  if(plotchoice=="y"){
    
    # Store quantile regressions as functions for plotting with
    # the stat_function() ggplot2 argument.
    phl <- function(x) coef(hdl)[1] + coef(hdl)[2]*x
    phm <- function(x) coef(hdm)[1] + coef(hdm)[2]*x
    phu <- function(x) coef(hdu)[1] + coef(hdu)[2]*x
    prl <- function(x) coef(rdl)[1] + coef(rdl)[2]*x
    prm <- function(x) coef(rdm)[1] + coef(rdm)[2]*x
    pru <- function(x) coef(rdu)[1] + coef(rdu)[2]*x
    ptl <- function(x) coef(tdl)[1] + coef(tdl)[2]*x
    ptm <- function(x) coef(tdm)[1] + coef(tdm)[2]*x
    ptu <- function(x) coef(tdu)[1] + coef(tdu)[2]*x
    pfl <- function(x) coef(fdl)[1] + coef(fdl)[2]*x
    pfm <- function(x) coef(fdm)[1] + coef(fdm)[2]*x
    pfu <- function(x) coef(fdu)[1] + coef(fdu)[2]*x
    
    phldf <- data.frame(x=exp(seq(log(0.1), max(humerus$ldepth), length.out=50)),
                        y=exp(phl(seq(log(0.1), max(humerus$ldepth), length.out=50))))
    phmdf <- data.frame(x=exp(seq(log(0.1), max(humerus$ldepth), length.out=50)),
                        y=exp(phm(seq(log(0.1), max(humerus$ldepth), length.out=50))))
    phudf <- data.frame(x=exp(seq(log(0.1), max(humerus$ldepth), length.out=50)),
                        y=exp(phu(seq(log(0.1), max(humerus$ldepth), length.out=50))))
    
    prldf <- data.frame(x=exp(seq(log(0.1), max(radius$ldepth), length.out=50)),
                        y=exp(prl(seq(log(0.1), max(radius$ldepth), length.out=50))))
    prmdf <- data.frame(x=exp(seq(log(0.1), max(radius$ldepth), length.out=50)),
                        y=exp(prm(seq(log(0.1), max(radius$ldepth), length.out=50))))
    prudf <- data.frame(x=exp(seq(log(0.1), max(radius$ldepth), length.out=50)),
                        y=exp(pru(seq(log(0.1), max(radius$ldepth), length.out=50))))
    
    ptldf <- data.frame(x=exp(seq(log(0.1), max(tibia$ldepth), length.out=50)),
                        y=exp(ptl(seq(log(0.1), max(tibia$ldepth), length.out=50))))
    ptmdf <- data.frame(x=exp(seq(log(0.1), max(tibia$ldepth), length.out=50)),
                        y=exp(ptm(seq(log(0.1), max(tibia$ldepth), length.out=50))))
    ptudf <- data.frame(x=exp(seq(log(0.1), max(tibia$ldepth), length.out=50)),
                        y=exp(ptu(seq(log(0.1), max(tibia$ldepth), length.out=50))))
    
    pfldf <- data.frame(x=exp(seq(log(0.1), max(femur$ldepth), length.out=50)),
                        y=exp(pfl(seq(log(0.1), max(femur$ldepth), length.out=50))))
    pfmdf <- data.frame(x=exp(seq(log(0.1), max(femur$ldepth), length.out=50)),
                        y=exp(pfm(seq(log(0.1), max(femur$ldepth), length.out=50))))
    pfudf <- data.frame(x=exp(seq(log(0.1), max(femur$ldepth), length.out=50)),
                        y=exp(pfu(seq(log(0.1), max(femur$ldepth), length.out=50))))
    
    # Create humerus metrics plot.
    humd <- ggplot(data=humerus, aes(depth, length)) + geom_point() +
      geom_line(data=phudf, aes(x=x, y=y), alpha=0.2, size=1) +
      geom_line(data=phldf, aes(x=x, y=y), alpha=0.2, size=1) +
      geom_line(data=phmdf, aes(x=x, y=y), alpha=0.2, size=1, linetype="dashed") +
      xlim(0, max(humerus$depth)) + 
      ylim(0, exp(phu(max(humerus$ldepth))))+
      annotate("text", label="humerus", x=0.1*max(humerus$depth), 
               y=0.9*exp(phu(max(humerus$ldepth))), size=5)+
      labs(x="Depth (mm)", y="Length (mm)")
    
    # Create radius metrics plot.
    radd <- ggplot(data=radius, aes(depth, length)) + geom_point() +
      geom_line(data=prudf, aes(x=x, y=y), alpha=0.2, size=1) +
      geom_line(data=prldf, aes(x=x, y=y), alpha=0.2, size=1) +
      geom_line(data=prmdf, aes(x=x, y=y), alpha=0.2, size=1, linetype="dashed") +
      xlim(0, max(radius$depth)) + 
      ylim(0, exp(pru(max(radius$ldepth))))+
      annotate("text", label="radius", x=0.1*max(radius$depth), 
               y=0.9*exp(pru(max(radius$ldepth))), size=5)+
      labs(x="Depth (mm)", y="Length (mm)")
    
    # create femur metrics plot.
    femd <- ggplot(data=femur, aes(depth, length)) + geom_point() +
      geom_line(data=pfudf, aes(x=x, y=y), alpha=0.2, size=1) +
      geom_line(data=pfldf, aes(x=x, y=y), alpha=0.2, size=1) +
      geom_line(data=pfmdf, aes(x=x, y=y), alpha=0.2, size=1, linetype="dashed") +
      xlim(0, max(femur$depth)) + 
      ylim(0, exp(pfu(max(femur$ldepth))))+
      annotate("text", label="femur", x=0.1*max(femur$depth), 
               y=0.9*exp(phu(max(femur$ldepth))), size=5)+
      labs(x="Depth (mm)", y="Length (mm)")
    
    # Create tibia metrics plot.
    tibd <- ggplot(data=tibia, aes(depth, length)) + geom_point() +
      geom_line(data=ptudf, aes(x=x, y=y), alpha=0.2, size=1) +
      geom_line(data=ptldf, aes(x=x, y=y), alpha=0.2, size=1) +
      geom_line(data=ptmdf, aes(x=x, y=y), alpha=0.2, size=1, linetype="dashed") +
      xlim(0, max(tibia$depth)*1.01) + 
      ylim(0, exp(ptu(max(tibia$ldepth)))*1.01)+
      annotate("text", label="tibia", x=0.1*max(tibia$depth), 
               y=0.9*exp(ptu(max(tibia$ldepth))), size=5)+
      labs(x="Depth (mm)", y="Length (mm)")
    
    # Plot the element relationships in a 2x2 figure.
    grid.arrange(humd, radd, femd, tibd, ncol=2)
    
    # Present text describing the plots
    cat("Solid grey lines show the 2.5% and 97.5% quantiles for the ",
        "\nrelationships. The dashed grey lines show median relationships.",
        "\nAll regressions take the form 'log(length) ~ a + b*log(depth)'.\n\n")
  }
  
  # Return data frame of regression coefficients.
  return(modelspar)
  
}


# FUNCTION: This function creates a csv table of bootstrapped
# antiquus/modern mean long-bone lengths. It takes no arguments 
# and does not return any variables.
bootratios <- function(){
  # Data frame of modern and antiquus bison metrics
  tb <- read.csv("adultbison.csv", header=TRUE, stringsAsFactors=FALSE)
  # Initialize table to store element lenth ratio summary stats
  elements <- c("humerus", "radius", "femur", "tibia")
  ratio.stats <- data.frame(elements, rep(0,4), rep(0,4), rep(0,4))
  names(ratio.stats) <- c("elements", "lb", "mean", "ub")
  
  # Ensure reproducible bootstrapped ratios
  set.seed(10)
  
  # For each element, bootstrap the mean length for each sex for
  # both modern bison and antiquus (5000 iterations). Calculate 
  # a vector of antiquus/modern length ratios for each sex based
  # on the bootstrapped mean vectors. Combine each sex specific
  # vector of ratios into one vector. From this combined vector,
  # find the 2.5%, 50% (mean), and 97.5% values. Store these in
  # the summary stats table.
  for (x in 1:length(elements)){
    etb <- tb[which(tb$element==elements[x]),]
    
    # Filter for most abundant element side
    maxside <- names(sort(table(etb$side),decreasing=TRUE))[1]
    etb <- etb[which(etb$side==maxside),]
    
    bmm <- boot(etb$metric[which(etb$sex=="male" & etb$form=="modern")],
                statistic = function(x, index) mean(x[index]), R = 1E4)
    bfm <- boot(etb$metric[which(etb$sex=="female" & etb$form=="modern")],
                statistic = function(x, index) mean(x[index]), R = 1E4)
    bma <- boot(etb$metric[which(etb$sex=="male" & etb$form=="antiq")],
                statistic = function(x, index) mean(x[index]), R = 1E4)
    bfa <- boot(etb$metric[which(etb$sex=="female" & etb$form=="antiq")],
                statistic = function(x, index) mean(x[index]), R = 1E4)
    
    maleratios <- bma$t/bmm$t
    femaleratios <- bfa$t/bfm$t
    ratios <- c(maleratios, femaleratios)
    
    ratio.stats[which(ratio.stats$elements==elements[x]),2] <- quantile(ratios, 0.025)
    ratio.stats[which(ratio.stats$elements==elements[x]),3] <- quantile(ratios, 0.500)
    ratio.stats[which(ratio.stats$elements==elements[x]),4] <- quantile(ratios, 0.975)
  }
  
  write.csv(ratio.stats, "lengthratios.csv")
}


# The following text blocks introduce the program to the user.
cat("\n\n**************************************************************************\n")
cat("* Probabilistic Seasonality Models for Fetal Bison antiquus Osteometrics *\n")
cat("**************************************************************************\n\n",
  "- Built in R version 3.5.1 -- 'Feather Spray'\n",
  "- Last updated on August 28, 2019.\n",
  "- Report bugs or make suggestions: rbreslawski@smu.edu\n",
  "- Required packages: ggplot2, gridExtra, reshape2, \n",
  "                     quantreg, boot, smoother\n",
  "This script provides numerical and graphical season of death estimates\n",
  "(SODEs) for fetal antiquus specimens based on radius, humerus, tibia,\n",
  "and femur osteometrics.\n\n")

line <- readline(prompt="Press any key to continue.")

cat("\n  The program will first prompt you to select a baseline distribution for the begin-\n",
  "ning of the gestation period. These distributions are based on observations of modern\n",
  "bison.\n\n")
cat("  After a baseline distribution for the beginning of the gestation period is selected.\n",
  "you will be prompted to enter the number of skeletal specimens to be analyzed as well\n",
  "as metric values (in mm) for the minimum diaphyseal depths of each specimen. Alterna-\n",
  "tively, you will be given the option to upload a csv table of elements and metric \n",
  "values. Afer metrics have been entered or uploaded, the program will plot and print\n",
  "SODES for each element.\n\n")
cat("  Following these SODEs, you will be presented with options for further analysis. One of\n",
  "these options allows you to specify a date interval for a 'season of death' hypothesis.\n",
  "The program returns a SODE based probability that elements fall within this hypothesized\n",
  "interval (with graphical output). Additionally, you may combine multiple elements from a\n",
  "single fetus for a more constrained gestation age, producing a narrower SODE.\n\n")

line <- readline(prompt="Press any key to begin.")

## Set global variables for plot objects that are reused in multiple functions.
p.month.name <- c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                  "Aug","Sep","Oct","Nov","Dec") # Month names
p.month.xpos <- c(15, 43, 74, 104, 135, 165, 196, 227, 257,
                  288, 318, 349) # x-positions for month names
p.x1 <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335) # Start x-positions for month geometry
p.x2 <- c(30, 58, 89, 119, 150, 180, 211, 242, 272, 303, 333, 364)  # End x-positions for month geometry

# Call function to run script.
main.function()

	