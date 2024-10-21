import pandas as pd

from utils import *
import json
from datetime import datetime, timedelta
from dbclient import DBClient
import warnings


class ActivityScraper:
    def __init__(self, cleanse_period=100):

        creds = None
        with open('api_credentials/strava_credentials.json') as f:
            creds = json.load(f)

        if not creds:
            raise ValueError("No credentials provided")

        self.clientID = creds["client_id"]
        self.clientSecret = creds["client_secret"]
        self.tokens = pd.read_csv("access_tokens.tsv", sep="\t", index_col=0)

        # Cleanse period given in days
        self.cleanse_period = cleanse_period

        self.mongo_client = DBClient()

        # for athlete_id in self.tokens.index:
        #     result = self.mongo_client.activity_db.find({"athlete_id": athlete_id})
        #     current_data = pd.DataFrame(list(result))
        #     print(athlete_id, current_data.shape[0])
        #
        # self.activity_url = "https://www.strava.com/api/v3/activities"
        # self.stream_keys = ["time", "distance", "altitude", "heartrate", "watts", "temp", "moving"]

    def update_token_db(self, force_local=False):
        for athlete in self.mongo_client.collections["athletes"].find():
            if athlete["accessToken"] is None or force_local:
                print(f"No access token for {athlete['stravaId']}, "
                      f"attempting to fetch from local table ...")

                if athlete["stravaId"] in self.tokens.index:
                    print("located local athlete tokens ...")
                    local_data = self.tokens.loc[athlete["stravaId"]]

                    query_filter = {'stravaId': athlete["stravaId"]}
                    update_operation = {
                        '$set':
                            {'accessToken': local_data["Access_token"],
                             'refreshToken': local_data["Refresh_token"],
                             'tokenExpiration': local_data["Expires_at"]}
                    }
                    self.mongo_client.collections["athletes"].update_one(query_filter, update_operation)

            else:
                # print("Checking if tokens are expired")
                if (athlete["tokenExpiration"] is None or
                        athlete["tokenExpiration"] < time.time()):
                    print("Token expired")

                    response = requests.post(
                        url='https://www.strava.com/oauth/token',
                        data={
                            'client_id': self.clientID,
                            'client_secret': self.clientSecret,
                            'grant_type': 'refresh_token',
                            'refresh_token': athlete["refreshToken"]
                        }
                    )
                    # Save response as json in new variable
                    new_strava_tokens = response.json()

                    query_filter = {'stravaId': athlete["stravaId"]}
                    update_operation = {
                        '$set':
                            {'accessToken': new_strava_tokens["access_token"],
                             'tokenExpiration': new_strava_tokens["expires_at"]}
                    }

                    self.mongo_client.collections["athletes"].update_one(query_filter, update_operation)

            # print(athlete["accessToken"], athlete["tokenExpiresAt"])

    def cleanse_old_data(self):
        cut_off_date = datetime.now() - timedelta(days=self.cleanse_period)

        activities = self.mongo_client.collections["activities"].find(
            {"start_date_local": {"$lte": cut_off_date.strftime("%Y-%m-%d")}})
        for activity in activities:
            print(activity["start_date_local"], activity["stravaActivityId"])

        self.mongo_client.collections["activities"].delete_many(
            {"start_date_local": {"$lte": cut_off_date.strftime("%Y-%m-%d")}})

    def update_athlete_activities(self, strava_id, force_continue=False, write_demo=False):
        url = "https://www.strava.com/api/v3/activities"

        athlete_data = self.mongo_client.collections["athletes"].find_one({"stravaId": strava_id})
        access_token = athlete_data["accessToken"]

        # Specify the keys to extract from the API request
        extract_fields = ["id", "name", "description", "distance", "moving_time", "elapsed_time",
                          "sport_type", "start_date_local", "available_zones"]

        # Get strava ids of current activities
        recorded_activities = self.mongo_client.collections["activities"].distinct("stravaActivityId")

        # Load new data from strava
        new_activities, page, new_count = True, 1, 0
        old_flag = False

        max_new_count, per_page = 50, 10

        uploads = []

        while new_activities and new_count < max_new_count and not old_flag:
            urlString = f"{url}?access_token={access_token}&per_page={per_page}&page={page}"
            request = requests.get(urlString)
            activities = request.json()
            if activities:
                for idx, activity in enumerate(activities):
                    try:
                        activity_id = activity["id"]

                        start_time = (activity["start_date_local"][:-1]).replace("T", " ")
                        start_time = datetime.strptime(start_time, '%Y-%m-%d %H:%M:%S')

                        old_activity = datetime.now() - start_time > timedelta(days=self.cleanse_period)

                        if old_activity:
                            old_flag = True

                        if (activity_id not in recorded_activities) and (not old_activity):
                            detailed_activity = self.get_activity_details(activity_id, access_token)

                            activity_data = {k: v for k, v in detailed_activity.items() if k in extract_fields}

                            activity_data["start_date_local"] = start_time
                            activity_data["stravaActivityId"] = activity_data.pop("id")
                            activity_data["athleteId"] = athlete_data["_id"]

                            uploads.append(activity_data)

                            recorded_activities.append(activity_id)

                            new_count += 1

                            if write_demo:
                                with open("help_files/example_activity_response.json", "w") as outfile:
                                    json.dump(detailed_activity, outfile, indent=4)
                                print("written")
                        else:
                            if not force_continue:
                                new_activities = False
                                break

                    except (TypeError, KeyError):
                        print(activities)
                        new_activities = False
                        break

                page += 1
                print(new_activities, new_count, old_flag)

        if uploads:
            self.mongo_client.collections["activities"].insert_many(uploads)

    def update_all_activities(self):
        strava_ids = self.mongo_client.collections["athletes"].distinct("stravaId")

        for strava_id in strava_ids:
            self.update_athlete_activities(strava_id, force_continue=False)

    def update_tokens(self):
        for athlete_ID in self.tokens.index:
            if pd.isna(self.tokens.loc[athlete_ID, "Expires_at"]) or \
                    self.tokens.loc[athlete_ID, "Expires_at"] < time.time():
                print(f"Refreshing tokens for {self.tokens.loc[athlete_ID, 'First_name']}")

                response = requests.post(
                    url='https://www.strava.com/oauth/token',
                    data={
                        'client_id': self.clientID,
                        'client_secret': self.clientSecret,
                        'grant_type': 'refresh_token',
                        'refresh_token': self.tokens.loc[athlete_ID, "Refresh_token"]
                    }
                )
                # Save response as json in new variable
                new_strava_tokens = response.json()

                self.tokens.loc[athlete_ID, "Access_token"] = new_strava_tokens["access_token"]
                self.tokens.loc[athlete_ID, "Expires_at"] = new_strava_tokens["expires_at"]

        self.tokens.to_csv("access_tokens.tsv", sep="\t")

    def get_activity_details(self, activity_id, token):
        url = "https://www.strava.com/api/v3/activities"
        urlString = f"{url}/{activity_id}?access_token={token}&include_all_efforts=true"

        request = requests.get(urlString)
        activity = request.json()
        return activity

    def get_activity_streams(self, activity_id, token):
        urlString = f"{self.activity_url}/{activity_id}/streams?access_token={token}"
        stream_data = pd.DataFrame(columns=self.stream_keys)
        for key in self.stream_keys:
            try:
                p = {'keys': key, 'key_by_type': True}
                request = requests.get(urlString, params=p)
                activity_stream = request.json()
                stream_data[key] = activity_stream[key]["data"]

            except Exception as e:
                pass
                # print(f"couldn't find data for {key}")

        # stream_data = stream_data.set_index("time", drop=True)
        return stream_data


if __name__ == "__main__":
    scraper = ActivityScraper()
    # scraper.update_tokens()
    scraper.update_token_db(force_local=False)
    scraper.cleanse_old_data()

    scraper.update_all_activities()
    # # Update Ben
    # scraper.update_athlete_activities(38807221, write_demo=False, force_continue=True)
    # # Update Sophie
    # scraper.update_athlete_activities(71665908, write_demo=False, force_continue=True)