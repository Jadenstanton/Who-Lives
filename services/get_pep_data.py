from datetime import datetime
import requests
import os

"""
functions to download pep csv from link
link needs to change based on current year
rewrite file name
write file to inputs/PEP_data

# Example usage
downloader = GetPepData()
downloader.get_allstates_agesex_pep()
downloader.get_allstates_charagegroups_pep()
downloader.get_agesex_pep()
downloader.get_charagegroups_pep()
"""

# TODO: Make file path accurate and not the services folder

class GetPepData:

    def __init__(self):
        self.year = str(datetime.today().year - 1)
        self.filepath = "services"
        # filepath = os.join.path("inputs", "PEP_data")
        os.makedirs(self.filepath, exist_ok=True)

    def get_data(self, url, filename):
        output_file = os.path.join(self.filepath, filename)
        response = requests.get(url)
        response.raise_for_status()
        with open(output_file, "wb") as file:
            file.write(response.content)
        print(f"File saved to {output_file}")

    def get_allstates_agesex_pep(self):
        url = f"https://www2.census.gov/programs-surveys/popest/datasets/2020-{self.year}/counties/asrh/cc-est{self.year}-agesex-all.csv"
        self.get_data(url, f"PEP{self.year}_agesex_alldata.csv")


    def get_allstates_charagegroups_pep(self):
        url = f"https://www2.census.gov/programs-surveys/popest/datasets/2020-{self.year}/counties/asrh/cc-est{self.year}-alldata.csv"
        self.get_data(url, f"PEP{self.year}charagegroups_allstates.csv")

    def get_agesex_pep(self):
        url = f"https://www2.census.gov/programs-surveys/popest/datasets/2020-{self.year}/counties/asrh/cc-est{self.year}-agesex-22.csv"
        self.get_data(url, f"PEP{self.year}_agesex.csv")


    def get_charagegroups_pep(self):
        url = f"https://www2.census.gov/programs-surveys/popest/datasets/2020-{self.year}/counties/asrh/cc-est{self.year}-alldata-22.csv"
        self.get_data(url, f"PEP{self.year}charagegroups.csv")

if __name__ == "__main__":
    downloader = GetPepData()
    downloader.get_allstates_agesex_pep()
    downloader.get_allstates_charagegroups_pep()
    downloader.get_agesex_pep()
    downloader.get_charagegroups_pep()