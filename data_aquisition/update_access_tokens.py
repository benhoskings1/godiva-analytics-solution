import pandas as pd
import requests
import json


def get_access_tokens(code):
    response = requests.post(
        url='https://www.strava.com/oauth/token',
        data={
            'client_id': 111130,
            'client_secret': 'b7fca9288af24aaa2f86f1b82f70f2baa54daeb2',
            'code': code,
            'grant_type': 'authorization_code'
        }
    )

    tokens = response.json()

    try:
        athlete_tokens = {
            "ID": tokens["athlete"]["id"],
            "Access_token": tokens["access_token"],
            "Refresh_token": tokens["refresh_token"]
        }

    except:
        athlete_tokens = tokens

    return athlete_tokens

# http://localhost/exchange_token?state=&code=&scope=read,activity:read_all,profile:read_all
codes = ["42b7fd5b2055a30686392ab9623475ab39923c83"]

access_data_path = "access_tokens.tsv"

access_data = pd.read_csv(access_data_path, sep="\t")

for code in codes:
    tokens = get_access_tokens(code)
    print(tokens)
    if tokens:
        access_data = pd.concat([access_data, pd.DataFrame([tokens])], ignore_index=True)

access_data.to_csv("access_tokens.tsv", sep="\t", index=False)

# http://localhost/exchange_token?state=&code=ae77752c347275d852d20e3a5c3188191c92d58d&scope=read,activity:read_all,profile:read_all
# http://localhost/exchange_token?state=&code=689f7da80c421be6f7178e9f185b633f409fac29&scope=read,activity:read_all,profile:read_all
# http://localhost/exchange_token?state=&code=28f0d656c7e2f1136d430da91174d3a8b665d1e0&scope=read,activity:read_all,profile:read_all
# http://localhost/exchange_token?state=&code=42b7fd5b2055a30686392ab9623475ab39923c83&scope=read,activity:read_all,profile:read_all