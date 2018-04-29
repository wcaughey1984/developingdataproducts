#################################################################################################################
# Title:    runApp                                                                                              #
# Purpose:  (Developing Data Products) Code which calls the project app for this course.                        #
# Author:   Billy Caughey                                                                                       #
# Date:     2018.04.28                                                                                          #
#################################################################################################################

#install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='williamcaugheyapps',
                          token='token',
                          secret='secret')

rsconnect::deployApp('C:/Users/Owner/Dropbox (Personal)/Coursera/Developing Data Products/Week 4/worldSeriesCounts')