# ADA.Simpson.textmining-

##Background

*The Simpsons* is an American animated sitcom created by Matt Groening for the Fox Broadcasting Company. The series is a satirical depiction of working-class life epitomized by the Simpson family, which consists of Homer, Marge, Bart, Lisa and Maggie. The show is set in the fictional town of Springfield and parodies America culture, society, television, and the human condition. Since its debut on December 17, 1989, 605 episodes of *The Simpsons* have been broadcast. It is the longest-running American sitcom and the longest-running American animated program.

*The Simpsons* was the Fox network’s first television series to rank among a season’s top 30 highest- rated shows. In 1990, Bart quickly became one of the most popular characters on television in what was termed “Bartmania”. On January 14, 2000, the Simpson family was awarded a star on the Hollywood Walk of Fame. Homer’s exclamatory catchphrase “D’oh!” has been adopted into English language. However, the show has also been criticized for what many perceive as a decline in quality over the years. In 2010, the BBC noted “the common consensus is that The Simpsons’ golden era ended after season nine”.

##Objective

There are significant decreasing trends for both U.S. viewers and rate. We are curious about the declines and want to find a solution to stop this decrease. Firstly, we want to find the change point of the rating. Secondly, we will find internal and external factors contributing to the decrease of rating. Last but not least, we will make some suggestions about how to make Simpson popular again.

##Data Source

The data source of whole project is from *The Simpsons* by the Data on Kaggle. The data was collected from December 17, 1989 to October 16, 2016, which contained all seasons results. There are two main data sets we apply during the research.
* **Episode:**This data contains 600 observations. The variables include title, air date, season, series, US viewers in millions, views and rating for each episode.
* **Script lines:**This document contains the dialogue spoken by each character in different locations in details.

We use the first dataset for time series and change point detection steps. The text mining method is based on both datasets.

##Method
We will use time series analysis, change point detection, and text mining methods to achieve the research proposal.
