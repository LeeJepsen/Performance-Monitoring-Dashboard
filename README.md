# Team Performance Monitoring Dashboard (R Shiny)

## Overview

This Project is an R Shiny Dashboard designed to support **team level Countermovement Jump (CMJ) performance monitoring** in an interpretable way, rather than presenting every available metric. This was developed with the use case of analyzing performance trends across a period of time.  

The application is intended as a **monitoring and communication tool**, not a diagnostic system.  The primary goal is to translate force plate outputs into a visual format that supports decision making between sport scientist, strength coaches, and performance staff. An example use case: Strength and Conditioning staff monitoring how their athletes are responding to training stimulus. In this example dashboard jump height and RSI-mod were chosen as KPIs to analyze training adaptations during preseason.  

- Jump height to monitor fatigue and response to training load
- RSI-mod (reactive strength index modified: jump height / time to takeoff) to add additional context to Jump height.  This could be movement strategy, fatigue, stretch shortening cycle utilization.  To paint a more informative picture of training adaptations.


## Data Disclaimer
The dataset included in this repository is an **example dataset exported from Vald ForceDecks**. The performance values are simulated to similar values seen by weekly athlete CMJ testing.  

Purpose of the example data is to demonstrate data flow, visualization, and dashboard functionality.


## Functionality & Features

This repository contains: The code to run the app in R, dataset, and screenshots can be found in subsequent folders. More information on how to run the app can be found in the "How-To" Folder. 

<img width="700" height="300" alt="image" src="https://github.com/user-attachments/assets/f4485dec-2c86-4046-ad8c-160c154146c7" />


**Main Tab: Team Overview**
- Select a team and jump type (in this case Team 1, and CMJ) with drop down features
- Variables descriptions seen within the dashboard. (Jump height, RSI-mod, Flagged Athletes, Smallest Worthwhile change (SWC))
- Last test date
- Average jump height of team, and its change from the first week of testing
- Average RSI-mod of team, and its change from the first week of testing
- Number of athletes tested
- Number of athletes flagged from most recent test, calculated by smallest worthwhile change (SWC), this may lead to closer monitoring or intervention
- Hover-able data points on both graphs showing: Date, performance, and # of athletes tested
- A grey standard deviation band on the jump height graph to see the team’s variance in performance for each week

<img width="700" height="400" alt="image" src="https://github.com/user-attachments/assets/a8af62e5-6763-4c57-be34-5cb520c97e08" />


**Second Tab: Athlete tables**
- Ability to select an athlete on the drop down tab; which populates corresponding athlete performance timeline table 
- Scrollable team summary table with each athletes personal best and last test date.  

