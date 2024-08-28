import pandas as pd
import requests
import time
from geopy.geocoders import Nominatim

app = Nominatim(user_agent="tutorial")

detailed_activity_fields = [
    "id", "name", "distance", "moving_time", "elapsed_time",
    "total_elevation_gain", "elev_high", "elev_low",
    "sport_type", "start_date", "start_date_local", "timezone",
    "start_latlang", "end_latlang", "achievement_count",
    "kudos_count", "comment_count", "athlete_count",
    "workout_type", "average_speed", "max_speed", "has_kudoed",
    "hide_from_home", "gear_id", "average_watts", "device_watts",
    "max_watts", "description", "device_name",
    "embed_token", "splits_metric", "splits-standard", "laps",
]

extract_activity_fields = [
    "id", "name", "distance", "moving_time", "elapsed_time",
    "total_elevation_gain", "elev_high", "elev_low",
    "sport_type", "start_date", "start_date_local", "manual", "private",
    "workout_type", "average_speed", "max_speed", "gear_id", "average_watts", "device_watts",
    "max_watts", "weighted_average_watts", "description", "device_name",
    "splits_metric", "splits-standard", "laps", "best_efforts",
    "segment_efforts"
]

# detailed_activity_fields = [
#     "id", "name", "distance", "moving_time", "elapsed_time",
#     "total_elevation_gain", "elev_high", "elev_low",
#     "sport_type", "start_date", "start_date_local", "timezone",
#     "start_latlang", "end_latlang", "achievement_count",
#     "kudos_count", "comment_count", "athlete_count",
#     "total_photo_count", "trainer", "commute", "manual", "private",
#     "flagged", "workout_type", "average_speed", "max_speed", "has_kudoed",
#     "hide_from_home", "gear_id", "kilojoules", "average_watts", "device_watts",
#     "max_watts", "weighted_average_watts", "description", "calories", "device_name",
#     "embed_token", "splits_metric", "splits-standard", "laps", "best_efforts",
#     "segment_efforts"
# ]

# "elev_high", "elev_low",
#     "sport_type", "start_date", "start_date_local", "manual", "private",
#     "flagged", "workout_type", "average_speed", "max_speed", "gear_id", "average_watts", "device_watts",
#     "max_watts", "weighted_average_watts", "description", "calories", "device_name",
#     "splits_metric", "splits-standard", "laps", "best_efforts",
#     "segment_efforts"

def get_activity_country(coords):
    # coords = coords.replace("[", "").replace("]", "").split(", ")
    # coords = [float(coord) for coord in coords]
    get_address_by_location(*coords)

    coords = f"{coords[0]}, {coords[1]}"

    data = app.reverse(coords, language="en").raw
    address = data["address"]
    return address["country"]


def string_to_list(str):
    if type(str) == 'string':
        str = str.replace("[", "").replace("]", "").split(", ")

    for idx, item in enumerate(str):
        str[idx] = float(str[idx])

    return str


def get_address_by_location(latitude, longitude, language="en"):
    """This function returns an address as raw from a location
    will repeat until success"""
    # build coordinates string to pass to reverse() function
    coordinates = f"{latitude}, {longitude}"
    # sleep for a second to respect Usage Policy
    try:
        return app.reverse(coordinates, language=language).raw
    except:
        time.sleep(1)
        return get_address_by_location(latitude, longitude)


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

class LocationData:
    def __init__(self, latlngString):
        latlang = string_to_list(latlngString)
        if latlang:
            data = get_address_by_location(latlang[0], latlang[1])
            address = data["address"]
            try:
                self.village = address["village"]
            except KeyError:
                self.village = None
            try:
                self.county = address["county"]
            except KeyError:
                self.county = None
            try:
                self.state = address["state"]
            except KeyError:
                self.state = None
            try:
                self.houseNumber = address["houseNumber"]
            except KeyError:
                self.houseNumber = None
            try:
                self.road = address["road"]
            except KeyError:
                self.road = None
            try:
                self.neighbourhood = address["neighbourhood"]
            except KeyError:
                self.neighbourhood = None
            try:
                self.suburb = address["suburb"]
            except KeyError:
                self.suburb = None

            self.country = address["country"]

        else:
            self.village = None
            self.county = None
            self.state = None
            self.houseNumber = None
            self.road = None
            self.neighbourhood = None
            self.suburb = None
            self.country = None