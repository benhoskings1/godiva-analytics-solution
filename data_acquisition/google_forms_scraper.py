

from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from googleapiclient.discovery import build

# https://docs.google.com/forms/d/1hhyFgTs4-tEs-uevfLreNWbb_vzyW94s0Kroap3OTAk/edit

SCOPES = ["https://www.googleapis.com/auth/forms.responses.readonly"]


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


if __name__ == "__main__":
    mgr = MemberManager()
