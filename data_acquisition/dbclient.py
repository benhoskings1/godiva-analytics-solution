from pymongo import MongoClient
import pandas as pd
from pymongo.mongo_client import MongoClient
from pymongo.server_api import ServerApi

class DBClient:
    def __init__(self):
        username = 'benjaminhoskings'
        password = 'i6UMmr7TpOcIOUcc'
        self.client = MongoClient(f'mongodb+srv://{username}:{password}@cluster0.w6iufqn.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0')
        self.collections = {
            "members": self.client.get_database("godiva_data").members,
            "athletes": self.client.get_database("godiva_data").athletes,
            "coaches": self.client.get_database("godiva_data").coaches,
            "activities": self.client.get_database("godiva_data").activities,
            "session_types": self.client.get_database("godiva_data").session_types,
            "access_types": self.client.get_database("godiva_data").access_types,
        }

        self.activity_db = self.client.get_database('godiva_data').activity_data
        self.member_data = self.client.get_database("godiva_data").athlete_data
        self.coaches = self.client.get_database("godiva_data").coaches


    def check_connection(self):
        try:
            self.client.admin.command('ping')
            print("Pinged your deployment. You successfully connected to MongoDB!")
        except Exception as e:
            print(e)

    def create_index(self):
        self.activity_db.create_index("athlete_id")

    def clear_all(self, db_name=None):
        print(db_name in self.collections)
        if db_name in self.collections:
            self.collections[db_name].delete_many({})
            print(f'All entries successfully cleared from {db_name} table.')
        else:
            print("Unable to locate the DB to clear")

    def trim_data(self):
        ...

    def upload_consult(self, new_consult):
        self.activity_db.insert_one(new_consult)
        print('New consultation data uploaded to MongoDB.')


if __name__ == "__main__":
    db_client = DBClient()
    db_client.check_connection()
    # db_client.create_index()

    db_client.clear_all("activity")

    # activity_data = {"athlete_id": 38807221, "activity_id": 12, "type": 3}
    # db_client.upload_consult(activity_data)
    #
    # activity_ids = [data["activity_id"] for data in db_client.activity_db.find()]


