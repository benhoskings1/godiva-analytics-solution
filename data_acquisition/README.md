# REPS - Data Acquisition

## Member data fields
Each member has five data fields associated with them 

```json
{"first_name": "John", 
  "last_name":  "Smith", 
  "dob": "DD/MM/YYYY", 
  "strava_id":  "XX...XX", 
  "event":  "800m"}
```

## Mongo DB
All data is stored securely in MongoDB

The database can only ba accessed by registered IPs. Run the following command in terminal to find your current ip address.
```commandline
dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com
```

## Automation of strava extraction

### Create bash script
Create a new text file named **update_godiva_data.sh** and add the following code:
```commandline
cd /Users/benhoskings/Documents/Pycharm/godiva-app/venv
source bin/activate
cd .. 
python3.10 activity_scraper_2.py
echo complete!
```

### Assign chrontab automation
To add a new chrontab, run the following command
```commandline
crontab -e
```
Press i to enter inset mode, then add the **crontab** command below to run the bash script every hour
```commandline
0 * * * * path_to_bash_script/update_godiva_data.sh
```

Below is Ben's script path :))
```commandline
0 * * * *  /Users/benhoskings/Desktop/update_godiva_data.sh
```
Press **esc** to exit insert mode. Type **:wq** and then press **enter**.  

The automation system is now set up.


