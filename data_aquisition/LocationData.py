import pandas as pd
import requests
import time
from geopy.geocoders import Nominatim

app = Nominatim(user_agent="tutorial")


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
