# Coventry Godiva Data Analytics Solution

This project is a full stack application that collects both qualitative and quantitative data about athletes, collating the 
data into a simple overview for coaches to monitor the training progress of each of their athletes. 

## Data pipelines
Raw data is imported primarily through the Strava API. This data is then supplemented by additional qualitative data from 
google forms (and thus the native API).

For both Strava and Google Forms, the Crontab scheduler is used to automate the running of a python script that scrapes
data from both input sources and send the data to MongoDB. 

## Database management
All data is held securely on the cloud using MongoDB. SQL is used to interface between python and the database.

## Data Processing
All data processing is 

# Progress Tracking

## 1. Please click this link:

<http://www.strava.com/oauth/authorize?client_id=111130&response_type=code&redirect_uri=http://localhost/exchange_token&approval_prompt=force&scope=profile:read_all,activity:read_all>

## 2. Log in to your strava account. 

## 3. Allow access to public activities (required)

### 3.1 Please also allow access to the other data (not required)

## 4. Press Authorize

## 5. Copy the link in your search bar

### Do not worry if the page looks like an error. THIS IS MEANT TO HAPPEN. 

(Should look something like this)

<http://localhost/exchange_token?state=&code=YOUR_CODE_IS_HERE&scope=read,activity:read_all,profile:read_all>

## 6. Send this link to Ben

### Email: Benjaminhoskings\@gmail.com

### Message/WhatApp: 07711981822
