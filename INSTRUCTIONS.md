# Reds_BatterPerformanceProjection
Reds Assessment 2

Project Instructions:

Your project is to predict how many singles, doubles, triples, and home runs each player in the test.csv will hit. The train.csv and test.csv have distinct differences. This assignment should take no more than 8 hours, so please do not spend longer than 8 total hours on this.

Each row is a single pitch that results in a plate appearance. In the train.csv dataset, you have information about the pitch thrown, the game state (outs, count, park), bat contact, previous season batter performance (slash line), and the result of the plate appearance (see the feature glossary at the bottom). For simplicity, there is bat-to-ball contact in each row (do not worry about swing-and-miss).

The test.csv has the same information as the train.csv except there is no result information and also no bat contact information. In other words, there is only information about the pitch thrown, the game state, and the batter's previous season performance.

Please use statistical/machine learning/deep learning models to predict the singles, doubles, triples, and home runs in the test.csv. The final result should be the probability of a single, double, triple, and home run for each row in the test.csv. Please use comments throughout your code to provide context and clarity for the reviewers.

You will be evaluated on methodology (31%), statistics usage (31%), communication/clarity (26%), data visualization (11%), and output accuracy/loss (1%).

How to submit
Your project should contain the code you used to produce your results and an updated output of test.csv that includes a probability for single, double, triple, and home run for each row.

Please zip compress your code and output and upload your completed project to a shared folder (Google Drive, OneDrive, box, etc), and then paste a link to the repository below in the form along with any comments you have about your solution. Please double check to make sure anyone with the link to your shared folder can access it.

Feature Glossary
‘BATTER_UID’: Unique anonymized ID for batter  
'EXIT_SPEED': speed off the bat  
'ANGLE': angle off the bat  
'DIRECTION': direction bearing off the bat  
'EVENT_RESULT_KEY': PA-related event  
'PITCH_RESULT_KEY': pitch-related event  
'PA': plate appearance  
'1B': single  
'2B': double  
'3B': triple  
'HR': home run  
'AVG': batter avg over the previous 3 seasons  
'OBP': on-base percentage over the previous 3 seasons  
'SLG': slugging rate over the previous 3 seasons  
'VENUE_KEY': ID for park  
'OUTS': outs in the inning  
'BALLS': balls in the count  
'STRIKES': strikes in the count  
'BATS_LEFT': 1 if the batter is a left-handed batter, 0 if not  
'THROWS_LEFT': 1 if the pitcher is a left-handed pitcher, 0 if not  
'PITCH_NUMBER': number of pitches in the at-bat  
'RELEASE_SPEED': speed of the pitch  
'PLATE_X': horizontal location of the pitch at the plate  
'PLATE_Z': vertical location of the pitch at the plate  
'INDUCED_VERTICAL_BREAK': vertical break of the pitch, accounting for gravity  
'HORIZONTAL_BREAK': horizontal break of the pitch  
'VERTICAL_APPROACH_ANGLE': vertical angle as a pitch is approaching the plate  
'HORIZONTAL_APPROACH_ANGLE': horizontal angle as a pitch is approaching the plate  
