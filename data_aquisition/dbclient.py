from pymongo import MongoClient
import pandas as pd
from pymongo.mongo_client import MongoClient
from pymongo.server_api import ServerApi

# uri = "mongodb+srv://benjaminhoskings:<password>@cluster0.w6iufqn.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"
#
# # Create a new client and connect to the server
# client = MongoClient(uri, server_api=ServerApi('1'))

# Send a ping to confirm a successful connection


class DBClient:
    def __init__(self):
        username = 'benjaminhoskings'
        password = 'i6UMmr7TpOcIOUcc'
        self.client = MongoClient(f'mongodb+srv://{username}:{password}@cluster0.w6iufqn.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0')
        self.activity_db = self.client.get_database('godiva_data').user_data
        self.member_data = self.client.get_database("godiva_data").athlete_data

    def check_connection(self):
        try:
            self.client.admin.command('ping')
            print("Pinged your deployment. You successfully connected to MongoDB!")
        except Exception as e:
            print(e)

    def create_index(self):
        self.activity_db.create_index("athlete_id")

    def clear_all(self, db_name=None):
        if db_name == "activity":
            self.activity_db.delete_many({})
            print('All activities successfully cleared from MongoDB.')
        elif db_name == "athlete":
            self.member_data.delete_many({})
        else:
            print("Please provide the name of the DB to clear")

    def trim_data(self):
        ...

    def upload_consult(self, new_consult):
        self.activity_db.insert_one(new_consult)
        print('New consultation data uploaded to MongoDB.')


if __name__ == "__main__":
    db_client = DBClient()
    db_client.check_connection()
    # db_client.create_index()

    # db_client.clear_all("activity")

    # activity_data = {"athlete_id": 38807221, "activity_id": 12, "type": 3}
    # db_client.upload_consult(activity_data)
    #
    # activity_ids = [data["activity_id"] for data in db_client.activity_db.find()]


