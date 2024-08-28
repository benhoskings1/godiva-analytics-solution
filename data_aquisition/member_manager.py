from utils import *
from dbclient import DBClient


class MemberManager:
    def __init__(self):
        self.mongo_client = DBClient()

    def add_member(self, data):
        self.mongo_client.member_data.insert_one(data)


if __name__ == "__main__":
    member_manager = MemberManager()
    member_manager.mongo_client.clear_all("athlete")

    ben_data = {"first_name": "Ben", "last_name": "Hoskings", "dob": "27/07/2002", "strava_id": 38807221, "group": "REPS"}
    neil_data = {"first_name": "Neil", "last_name": "Hoskings", "dob": "14/08/1964", "strava_id": 963223, "group": ""}
    becky_data = {"first_name": "Becky", "last_name": "Hoskings", "dob": "13/03/1972", "strava_id": 98588012, "group": ""}
    clarky_data = {"first_name": "Clark", "last_name": "Roberts", "dob": "", "strava_id": 38781770, "group": "REPS"}
    tom_data = {"first_name": "Tom", "last_name": "Beesley", "dob": "", "strava_id": 33370063, "group": "REPS"}
    sophie_data = {"first_name": "Sophie", "last_name": "Hurst", "dob": "", "strava_id": 71665908, "group": "REPS"}
    wais_data = {"first_name": "Wais", "last_name": "Yip", "dob": "", "strava_id": 14188705, "group": "REPS"}

    member_data = [ben_data, neil_data, becky_data, clarky_data, tom_data, sophie_data, wais_data]

    for member in member_data:
        member_manager.add_member(member)
