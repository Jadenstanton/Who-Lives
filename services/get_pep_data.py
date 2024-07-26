from datetime import datetime
import requests
import os

"""
functions to download pep csv from link
link needs to change based on current year
rewrite file name
write file to inputs/PEP_data
"""


def get_allstates_agesex_pep():
    current_date = datetime.today()
    year = str(current_date.year - 1)
    filepath = "services"
    # filepath = os.join.path("inputs", "PEP_data")
    os.makedirs(filepath, exist_ok=True)

    url = f"https://www2.census.gov/programs-surveys/popest/datasets/2020-{year}/counties/asrh/cc-est{year}-agesex-all.csv"

    output_file = os.path.join(filepath, f"PEP{year}agesex_allstates.csv")

    response = requests.get(url)
    response.raise_for_status()

    with open(output_file, "wb") as file:
        file.write(response.content)
    print(f"File saved to {output_file}")

def get_allstates_charagegroups_pep():
    current_date = datetime.today() 
    year = str(current_date.year - 1)
    filepath = "services"
    # filepath = os.join.path("inputs", "PEP_data")
    os.makedirs(filepath, exist_ok=True)

    url = f"https://www2.census.gov/programs-surveys/popest/datasets/2020-{year}/counties/asrh/cc-est{year}-alldata.csv"

    output_file = os.path.join(filepath, f"PEP{year}_agesex_allstates.csv")

    response = requests.get(url)
    response.raise_for_status()

    with open(output_file, "wb") as file:
        file.write(response.content)
    print(f"File saved to {output_file}")

def get_agesex_pep():
    current_date = datetime.today() 
    year = str(current_date.year - 1)
    filepath = "services"
    # filepath = os.join.path("inputs", "PEP_data")
    os.makedirs(filepath, exist_ok=True)

    url = f"https://www2.census.gov/programs-surveys/popest/datasets/2020-{year}/counties/asrh/cc-est{year}-agesex-22.csv"

    output_file = os.path.join(filepath, f"PEP{year}charagegroups.csv")

    response = requests.get(url)
    response.raise_for_status()

    with open(output_file, "wb") as file:
        file.write(response.content)
    print(f"File saved to {output_file}")

def get_charagegroups_pep():
    current_date = datetime.today()
    year = str(current_date.year - 1)
    filepath = "services"
    # filepath = os.join.path("inputs", "PEP_data")
    os.makedirs(filepath, exist_ok=True)

    url = f"https://www2.census.gov/programs-surveys/popest/datasets/2020-{year}/counties/asrh/cc-est{year}-alldata-22.csv"

    output_file = os.path.join(filepath, f"PEP{year}_agesex.csv")

    response = requests.get(url)
    response.raise_for_status()

    with open(output_file, "wb") as file:
        file.write(response.content)
    print(f"File saved to {output_file}")


get_allstates_charagegroups_pep()
get_allstates_agesex_pep()
get_charagegroups_pep()
get_agesex_pep()