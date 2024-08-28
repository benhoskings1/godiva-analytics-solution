import pandas as pd

from utils import *
from datetime import datetime, timedelta
from dbclient import DBClient
import warnings


class ActivityScraper:
    def __init__(self):
        self.clientID = 111130
        self.clientSecret = 'b7fca9288af24aaa2f86f1b82f70f2baa54daeb2'
        self.tokens = pd.read_csv("access_tokens.tsv", sep="\t", index_col=0)
        # self.allActivities = pd.read_csv("data/Activities.tsv", sep="\t", index_col="Activity_ID")

        self.mongo_client = DBClient()
        self.athlete_data = self.mongo_client.activity_db.find()

        for athlete_id in self.tokens.index:
            result = self.mongo_client.activity_db.find({"athlete_id": athlete_id})
            current_data = pd.DataFrame(list(result))
            print(athlete_id, current_data.shape[0])

        self.activity_url = "https://www.strava.com/api/v3/activities"
        self.stream_keys = ["time", "distance", "altitude", "heartrate", "watts", "temp", "moving"]

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

    def update_athlete_data(self, athlete_id):
        print("Updating athlete activities")
        url = "https://www.strava.com/api/v3/activities"

        access_token = self.tokens.loc[athlete_id, "Access_token"]
        result = self.mongo_client.activity_db.find({"athlete_id": athlete_id})
        current_data = pd.DataFrame(list(result), columns=detailed_activity_fields + ["athlete_id", "activity_id", "country"])

        current_data = current_data.sort_values(by=["start_date"], ascending=False)
        # print(current_data.head(10))

        min_date = datetime.now() - timedelta(weeks=10)

        new_activities = True
        page = 1
        newCount = 0

        warn_flag = False

        while new_activities:
            urlString = f"{url}?access_token={access_token}&per_page=10&page={page}"
            request = requests.get(urlString)
            activities = request.json()
            if activities:
                for idx, activity in enumerate(activities):
                    try:
                        activity_id = activity["id"]
                        start_time = (activity["start_date"][:-1]).replace("T", " ")
                        start_time = datetime.strptime(start_time, '%Y-%m-%d %H:%M:%S')

                        if activity_id not in current_data["activity_id"].values and start_time > min_date:

                            activity_data = self.get_activity_details(activity_id,
                                                                      self.tokens.loc[athlete_id, "Access_token"])
                            stream_data = self.get_activity_streams(activity_id,
                                                                    self.tokens.loc[athlete_id, "Access_token"])

                            activity_data["start_date"] = start_time

                            try:
                                if activity_data["start_latlng"]:
                                    country = get_activity_country(activity_data["start_latlng"])
                                else:
                                    country = None

                            except KeyError:
                                country = None

                            activity_data["athlete_id"] = activity_data["athlete"]["id"]
                            activity_data["country"] = country
                            activity_data["activity_id"] = activity_data["id"]

                            activity_data["data_streams"] = stream_data.to_dict(orient="list")

                            self.mongo_client.upload_consult(
                                {k: activity_data.get(k, None) for k in extract_activity_fields}
                            )

                            newCount += 1
                            new_activities = True
                        else:
                            new_activities = False

                    except (TypeError, KeyError):
                        print(activities)
                        new_activities = False
                        warn_flag = True
                        break

            else:
                new_activities = False

            page += 1

        if not warn_flag:
            print(f"Activities up to date (Added: {newCount})")
        else:
            warnings.warn("Problem scraping your activities. Please try again soon")

    def get_activity_details(self, activity_id, token):
        url = "https://www.strava.com/api/v3/activities"
        urlString = f"{url}/{activity_id}?access_token={token}&include_all_efforts=true"

        request = requests.get(urlString)
        activity = request.json()
        return activity

    def update_all_athletes(self):
        for athlete_id in self.tokens.index:
            self.update_athlete_data(athlete_id)

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
    scraper.update_tokens()
    # scraper.update_athlete_data(38807221)
    scraper.update_all_athletes()

