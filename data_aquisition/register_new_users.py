import os.path
import pandas as pd
import requests
from utils import get_access_tokens
import sys

from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError

SCOPES = ["https://www.googleapis.com/auth/forms.responses.readonly"]

# The ID of a sample document.
google_form_ids = {"register": "162rGRdf_Mk7vkZ48DjzA2YG3l7XF_-r7vzuXqG4lUAw",
                   "session_input": "1hhyFgTs4-tEs-uevfLreNWbb_vzyW94s0Kroap3OTAk"}

google_form_question_map = {
    "register": {"First Name": "345f9630", "Last Name": "353d4ddb", "URL": "19cfb4d2"},
    "session_input": {"name": "25e5793a", "date": "27f9e76d",
                      "other_info": "65040767", "rep_info": "3d9f1f9b"}
}

FORM_ID = "162rGRdf_Mk7vkZ48DjzA2YG3l7XF_-r7vzuXqG4lUAw"


class MemberManager:
    def __init__(self):
        self.creds = Credentials.from_authorized_user_file("api_credentials/forms_api_token.json", SCOPES)

        if not self.creds.valid:
            print("updating creds")
            if self.creds and self.creds.expired and self.creds.refresh_token:
                self.creds.refresh(Request())
            else:
                flow = InstalledAppFlow.from_client_secrets_file(
                    "api_credentials/credentials.json", SCOPES
                )
                self.creds = flow.run_local_server(port=0)

            # Save the credentials for the next run
            with open("api_credentials/forms_api_token.json", "w") as token:
                print("writing_token")
                token.write(self.creds.to_json())

        self.form_responses = None

    def get_responses(self, form_name):
        service = build("forms", "v1", credentials=self.creds)

        result = service.forms().responses().list(formId=google_form_ids[form_name]).execute()

        question_name_map = google_form_question_map[form_name]

        response_data = {}
        for response in result["responses"]:
            # print(response)
            user_data = {}
            for item in question_name_map.items():
                # print(response)
                data = response["answers"][item[1]]["textAnswers"]["answers"][0]["value"]
                user_data[item[0]] = data

            response_data[response["responseId"]] = user_data

        return pd.DataFrame(response_data).transpose()

    def update_member_tokens(self):
        access_data_path = "access_tokens.tsv"

        access_data = pd.read_csv(access_data_path, sep="\t")
        access_data.set_index("ID", inplace=True)

        responses = self.get_responses("register")
        responses["First_name"] = responses["First Name"]
        responses["Last_name"] = responses["Last Name"]
        responses = responses.drop(["First Name", "Last Name"], axis=1)

        for response_id in responses.index:
            response = responses.loc[response_id]
            # print(response.to_dict())
            try:
                parts = response["URL"].split("&")
                code = parts[1][5:]
                tokens = get_access_tokens(code)
                if tokens:
                    if "message" in tokens.keys():
                        # The code is not valid - has been used or is expired
                        print("link extraction failed")
                    else:
                        responses.loc[response_id, "ID"] = tokens["ID"]
                        responses.loc[response_id, "Refresh_token"] = tokens["Refresh_token"]
                        responses.loc[response_id, "Access_token"] = tokens["Access_token"]

                        # Possibly throwing warning!!!
                        responses.loc[response_id, "Expires_at"] = pd.NA

                        responses = responses.loc[:, ~responses.columns.str.contains('^Unnamed')]

            except Exception as e:
                # whole response is invalid
                # raise e
                print("link extraction failed")

        if (responses.columns.str.contains('ID')).any():
            responses = responses.set_index("ID")
            # print(responses)
            # responses = responses.loc[]
            access_data = pd.concat([access_data, responses], join='inner')
            access_data = access_data.loc[access_data.index.dropna()]

            access_data.to_csv("access_tokens.tsv", sep="\t", index=True)

            print("Access file updated!")


if __name__ == "__main__":
    manager = MemberManager()

    # # print(len(sys.argv))
    # if len(sys.argv) > 1:
    #     if sys.argv[1] == "register":
    #         manager.update_member_tokens()
    #     else:
    #         print(manager.get_responses(sys.argv[1]).to_string(index=False))
    #
    # else:
    #     print("Please provide the name of the form")
    # print(manager.get_responses("register").to_string(index=False))
    #




